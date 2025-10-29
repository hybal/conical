const std = @import("std");
const Hir = @import("Hir.zig");
const Ast = @import("Ast.zig");
const types = @import("types.zig");
const diag = @import("diag.zig");
const llvm = @import("llvm");

allocator: std.mem.Allocator,
context: *types.Context,
llvm_context: struct {
    builder: llvm.Core.LLVMBuilderRef,
    context: llvm.Core.LLVMContextRef,
    target_machine: llvm.TargetMachine.LLVMTargetMachineRef,
    target_data: llvm.Target.LLVMTargetDataRef,
    data_layout: llvm.TargetMachine.LLVMTargetDataRef,
    module: llvm.Core.LLVMModuleRef,
    current_function: llvm.Core.LLVMValueRef = null,
    pm: llvm.Core.LLVMPassManagerRef,
    target_size: u8,
},
hir_info_table: *Hir.HirInfoTable,
llvm_id_table: std.AutoHashMap(types.DefId, llvm.Core.LLVMValueRef),
name_index: usize = 0,
current_scope_id: usize = 0,
at_global: bool = true,

fn to_zstring(string: []const u8, allocator: std.mem.Allocator) ![*:0]const u8 {
    return try allocator.dupeZ(u8, string);
}

fn gen_name(self: *@This(), prefix: []const u8) ![*:0]const u8 {
    const name = try std.fmt.allocPrintZ(self.allocator, "{s}{d}", .{prefix, self.name_index});
    self.name_index += 1;
    return name;
}

fn add_llvm_symbol(self: *@This(), defid: types.DefId, value: llvm.Core.LLVMValueRef) !void {
    try self.llvm_id_table.put(defid, value);
}

fn get_llvm_symbol(self: *@This(), defid: types.DefId) llvm.Core.LLVMValueRef {
    return self.llvm_id_table.get(defid).?;
}


pub fn init(context: *types.Context, hir_info: *Hir.HirInfoTable, triple: [*]const u8, allocator: std.mem.Allocator) !@This() {
    llvm.Target.LLVMInitializeAllTargetInfos();
    llvm.Target.LLVMInitializeAllTargets();
    llvm.Target.LLVMInitializeAllTargetMCs();
    llvm.Target.LLVMInitializeAllAsmParsers();
    llvm.Target.LLVMInitializeAllAsmPrinters();
    var target: llvm.TargetMachine.LLVMTargetRef = null;
    var err: ?[*c]u8 = null;
    if (llvm.TargetMachine.LLVMGetTargetFromTriple(triple, &target, @ptrCast(&err)) != 0) {
        std.debug.print("LLVM Failed to create target: {s}", .{err.?});
        llvm.Core.LLVMDisposeMessage(err.?);
        return error.LLVMTargetFailed;
    }
    const target_machine = llvm.TargetMachine.LLVMCreateTargetMachine(
        target,
        triple,
        "generic",
        "",
        llvm.TargetMachine.LLVMCodeGenLevelDefault,
        llvm.TargetMachine.LLVMRelocPIC,
        llvm.TargetMachine.LLVMCodeModelDefault,
    );

    const llvmcontext = llvm.Core.LLVMGetGlobalContext();
    const builder = llvm.Core.LLVMCreateBuilderInContext(llvmcontext);
    const data_layout = llvm.TargetMachine.LLVMCreateTargetDataLayout(target_machine);
    const layout_str = llvm.TargetMachine.LLVMCopyStringRepOfTargetData(data_layout);
    //defer llvm.Core.LLVMDisposeMessage(layout_str);
    const module = llvm.Core.LLVMModuleCreateWithNameInContext("main", llvmcontext);
    const target_data = llvm.Target.LLVMCreateTargetData(layout_str);
    const pm = llvm.Core.LLVMCreateFunctionPassManagerForModule(module);
    llvm.Core.LLVMSetTarget(module, triple);
    llvm.Core.LLVMSetDataLayout(module, layout_str);
    llvm.Core.LLVMDisposeMessage(layout_str);
    return .{
        .llvm_context = .{
            .builder = builder,
            .context = llvmcontext,
            .target_machine = target_machine,
            .target_data = target_data,
            .data_layout = data_layout,
            .module = module,
            .pm = pm,
            .target_size = @intCast(llvm.Target.LLVMPointerSize(target_data) * 8),
        },
        .context = context,
        .allocator = allocator,
        .hir_info_table = hir_info,
        .llvm_id_table = .init(allocator),
    };
}
fn get_symbol(self: *@This(), defid: types.DefId) ?types.Symbol {
    var current_scope = self.context.sym_tab.items[self.current_scope_id];
    var exit = false;
    while (!exit) {
        if (current_scope.symbol_map.get(defid)) |outsym| {
            return outsym;
        }
        if (current_scope.parent) |parent| {
            current_scope = self.context.sym_tab.items[parent];
        } else {
            exit = true;
        }
    }
    return null;
}


pub fn emit(self: *@This(), hir: []Hir.Hir) !void {
    try self.lower_global(hir);
    self.at_global = false;
    for (hir) |node| {
        _ = try self.lower_local(node);
    }

    llvm.Core.LLVMDumpModule(self.llvm_context.module);
    //var err: [*c]u8 = (try self.allocator.alloc(u8, 5000)).ptr;
    //const target_machine = self.llvm_context.target_machine;
    //const module = self.llvm_context.module;
    //const file_name = try to_zstring(std.fs.path.basename(self.context.file_path), self.allocator);
    //const object_file = llvm.TargetMachine.LLVMObjectFile;
    //_ = llvm.TargetMachine.LLVMTargetMachineEmitToFile(
    //    target_machine, 
    //    @ptrCast(module), 
    //    file_name,
    //    object_file, 
    //    &err
    //);
    //std.debug.print("DEBUG: {s}\n", .{err});
}

fn createTypeRef(self: *@This(), ty: Ast.Type,) !llvm.Core.LLVMTypeRef {
    var base_type: llvm.Core.LLVMTypeRef = null;
    switch (ty.base_type) {
        .primitive => |prim| {
            switch (prim) {
                .I8, .U8, .Rune=>  base_type = llvm.Core.LLVMInt8TypeInContext(self.llvm_context.context),
                .I16, .U16 => base_type = llvm.Core.LLVMInt16TypeInContext(self.llvm_context.context),
                .I32, .U32=> base_type = llvm.Core.LLVMInt32TypeInContext(self.llvm_context.context),
                .I64, .U64 => base_type = llvm.Core.LLVMInt64TypeInContext(self.llvm_context.context),
                .I128, .U128 => base_type = llvm.Core.LLVMInt128TypeInContext(self.llvm_context.context),
                .ISize, .USize => {
                    const ptr_size = llvm.TargetMachine.LLVMPointerSize(self.llvm_context.data_layout) * 8;
                    base_type = llvm.Core.LLVMIntTypeInContext(self.llvm_context.context, ptr_size);
                },
                .F32 => base_type = llvm.Core.LLVMFloatTypeInContext(self.llvm_context.context),
                .F64 => base_type = llvm.Core.LLVMDoubleTypeInContext(self.llvm_context.context),
                .Bool => base_type = llvm.Core.LLVMInt1TypeInContext(self.llvm_context.context),
                .Never, .Unit => base_type = llvm.Core.LLVMVoidTypeInContext(self.llvm_context.context),
            }
        },
        .strct => |strct| {
            var iter = strct.fields.valueIterator();
            var arr = std.ArrayList(llvm.Core.LLVMTypeRef).init(self.allocator);
            while (iter.next()) |next| {
                const llvm_type = try self.createTypeRef(self.context.type_tab.get(next.*).?);
                try arr.append(llvm_type);
            }
            const slc = try arr.toOwnedSlice();
            base_type = llvm.Core.LLVMStructTypeInContext(self.llvm_context.context, slc.ptr, @intCast(slc.len), 0);
        },
        .@"enum" => |enm| {
            var max_size: c_ulonglong = 0;
            var iter = enm.variants.valueIterator();
            while (iter.next()) |next| {
                if (next.*) |nxt| {
                    const nxt_ty = self.context.type_tab.get(nxt).?;
                    const llvm_ty = try self.createTypeRef(nxt_ty);
                    const size = llvm.Target.LLVMABISizeOfType(self.llvm_context.target_data, @ptrCast(llvm_ty));
                    if (size > max_size) max_size = size;

                }
            }
            const storage_ty = llvm.Core.LLVMArrayType(
                try self.createTypeRef(Ast.Type.createPrimitive(.I8, null)), 
                @intCast(max_size)
            );
            const tag_ty = try self.createTypeRef(
                Ast.Type.createPrimitive(
                    Ast.PrimitiveType.from_int(@intCast(enm.variants.count()), false),
                    null
                )
            );

            if (max_size > 0) {
                const tagged_union = llvm.Core.LLVMStructTypeInContext(self.llvm_context.context,
                    @constCast(&[_]llvm.Core.LLVMTypeRef{tag_ty, storage_ty}),
                    2,
                    0
                );
                base_type = tagged_union;
            } else {
                base_type = tag_ty;
            }
        },
        .func => |fnc| {
            var fnc_args = try std.ArrayList(llvm.Core.LLVMTypeRef).initCapacity(self.allocator, fnc.args.len);
            for (fnc.args) |argid| {
                const arg = self.context.type_tab.get(argid).?;
                try fnc_args.append(try self.createTypeRef(arg));
            }
            const fnc_args_slice = try fnc_args.toOwnedSlice();
            const ret_ty = try self.createTypeRef(self.context.type_tab.get(fnc.ret).?);
            base_type = llvm.Core.LLVMFunctionType(
                ret_ty, 
                fnc_args_slice.ptr, 
                @intCast(fnc_args_slice.len), 
            0);
        },
        else => unreachable,
    }
    if (ty.modifiers) |mods| {
        if (mods.len > 0) {
            var i: usize = mods.len;
            while (i > 0) : ( i -= 1 ){
                switch (mods[i - 1]) {
                    .Ref, .RefMut, .RefConst, .Ptr, .PtrMut, .PtrConst => {
                        base_type = llvm.Core.LLVMPointerType(base_type, 0);
                    },
                    .Slice => {
                        const ptr_type = llvm.Core.LLVMPointerType(base_type, 0);
                        base_type = llvm.Core.LLVMStructType(
                            @constCast(&[_]llvm.Core.LLVMTypeRef{ ptr_type, llvm.Core.LLVMIntType(self.llvm_context.target_size) }),
                            2,
                            0
                        );
                    },
                    else => unreachable,
                }
            }
        }
    }
    return base_type;
}



fn lower_global(self: *@This(), hir: []Hir.Hir) !void {
    for (hir) |node| {
        const hir_info = self.hir_info_table.get(node.id).?;
        const ty = self.context.type_tab.get(hir_info.ty.?).?;
        const llvm_ty = try self.createTypeRef(ty);
        _ = llvm_ty;
        switch (node.node) {
            .top_level => {
                switch (node.node.top_level) {
                    .func => |decl| {
                        const ident = self.get_symbol(decl.id).?.name;
                        const name = try to_zstring(ident.value, self.allocator);
                        const fnc_ty = self.context.type_tab.get(decl.ty).?;
                        const llvm_fnc_ty = try self.createTypeRef(fnc_ty);
                        const llvm_func_ref = llvm.Core.LLVMAddFunction(
                            self.llvm_context.module,
                            name,
                            llvm_fnc_ty,
                        );
                        if (decl.is_extern) {
                            llvm.Core.LLVMSetLinkage(
                                llvm_func_ref,
                                llvm.Core.LLVMExternalLinkage,
                            );
                        }
                        try self.add_llvm_symbol(decl.id, llvm_func_ref);
                    },
                    else => return,
                }
            },
            else => return,
        }
    }
}


fn lower_local(self: *@This(), node: Hir.Hir) !llvm.Core.LLVMValueRef {
    const hir_info = self.hir_info_table.get(node.id).?;
    const ty = self.context.type_tab.get(hir_info.ty.?).?;
    const llvm_ty = try self.createTypeRef(ty);
    switch (node.node) {
        .top_level => {
            switch (node.node.top_level) {
                .func => |func| {
                    var llvm_id: llvm.Core.LLVMValueRef = undefined;
                    if (!self.at_global) {
                        const ident = self.get_symbol(func.id).?.name;
                        const name = try to_zstring(ident.value, self.allocator);
                        const fnc_ty = self.context.type_tab.get(func.ty).?;
                        const llvm_fnc_ty = try self.createTypeRef(fnc_ty);

                        const llvm_func_ref = llvm.Core.LLVMAddFunction(
                            self.llvm_context.module,
                            name,
                            llvm_fnc_ty,
                        );
                        if (func.is_extern) {
                            llvm.Core.LLVMSetLinkage(
                                llvm_func_ref,
                                llvm.Core.LLVMExternalLinkage,
                            );
                        }
                        llvm_id = llvm_func_ref;

                    } else {
                        llvm_id = self.get_llvm_symbol(func.id);
                    }
                    if (func.body) |body| {
                        const body_info = self.hir_info_table.get(body.id).?;
                        const saved_scope = self.current_scope_id;
                        self.current_scope_id = body_info.scope_id;
                        const entry = llvm.Core.LLVMAppendBasicBlockInContext(
                            self.llvm_context.context,
                            llvm_id,
                            "entry"
                        );
                        llvm.Core.LLVMPositionBuilderAtEnd(
                            self.llvm_context.builder,
                            entry
                        );
                        const out = try self.lower_local(body);
                        self.current_scope_id = saved_scope;
                        return out;
                    }
                    return null;

                },
                .binding => |bind| {
                    if (bind.is_mutable) {
                        const name = try self.gen_name("tmp");
                        const value_ref = llvm.Core.LLVMBuildAlloca(
                            self.llvm_context.builder,
                            llvm_ty,
                            name,
                        );
                        try self.add_llvm_symbol(bind.id, value_ref);
                        const val_ref = try self.lower_local(bind.expr);
                        const store = llvm.Core.LLVMBuildStore(
                            self.llvm_context.builder,
                            val_ref,
                            value_ref
                        );
                        return store;
                    }
                    const val_ref = try self.lower_local(bind.expr);
                    try self.add_llvm_symbol(bind.id, val_ref);
                    return val_ref;

                },
                .terminated => |expr| {
                    return try self.lower_local(expr.*);
                },
                .return_stmt => |stmt| {
                    const ret_expr = try self.lower_local(stmt.expr);
                    return llvm.Core.LLVMBuildRet(self.llvm_context.builder, ret_expr);
                },
                else => unreachable,
            }
        },
        .inline_expr => {
            switch (node.node.inline_expr) {
                .terminal => |term| {
                    switch (term.*) {
                        .integer_literal => |val| {
                            return llvm.Core.LLVMConstInt(llvm_ty, @intCast(val), 0);
                        },
                        .float_literal => |val| {
                            return llvm.Core.LLVMConstReal(llvm_ty, val);
                        },
                        .char_literal => |val| {
                            return llvm.Core.LLVMConstInt(llvm_ty, @intCast(val), 0);
                        },
                        .string_literal => |val| {
                            return llvm.Core.LLVMBuildGlobalStringPtr(
                                self.llvm_context.builder,
                                try to_zstring(val, self.allocator),
                                try self.gen_name("gblstr"),
                            );
                        },
                        .bool_literal => |val| {
                            return llvm.Core.LLVMConstInt(llvm_ty, if (val) 1 else 0, 0);
                        },
                        .path => |val| {
                            const sym = self.get_symbol(val).?;
                            _ = sym;
                            return self.get_llvm_symbol(val);
                        },
                        else => |unknown| {
                            std.debug.print("Unhandled switch case: {s}\n", .{ @tagName(unknown) });
                            unreachable;
                        }
                    }
                },
                .binary_expr => |expr| {
                    const lhs = try self.lower_local(expr.lhs);
                    const rhs = try self.lower_local(expr.rhs);
                    switch (expr.op) {
                        .Add => return llvm.Core.LLVMBuildAdd(
                            self.llvm_context.builder, 
                            lhs, 
                            rhs, 
                            @ptrCast(try self.gen_name("tmp"))
                        ),
                        .Sub => return llvm.Core.LLVMBuildSub(
                            self.llvm_context.builder, 
                            lhs, 
                            rhs, 
                            @ptrCast(try self.gen_name("tmp"))
                        ),
                        .Mul => return llvm.Core.LLVMBuildMul(
                            self.llvm_context.builder, 
                            lhs, 
                            rhs, @ptrCast(try self.gen_name("tmp"))
                        ),
                        .Div => {
                            if (ty.is_signed_int()) {
                                return llvm.Core.LLVMBuildSDiv(
                                    self.llvm_context.builder, 
                                    lhs, 
                                    rhs, 
                                    @ptrCast(try self.gen_name("tmp"))
                                );
                            } else if (ty.is_unsigned_int()) {
                                return llvm.Core.LLVMBuildUDiv(
                                    self.llvm_context.builder, 
                                    lhs, 
                                    rhs, 
                                    @ptrCast(try self.gen_name("tmp"))
                                );
                            }
                        },
                        .Mod => {
                            if (ty.is_signed_int()) {
                                return llvm.Core.LLVMBuildSRem(
                                    self.llvm_context.builder, 
                                    lhs, 
                                    rhs, @ptrCast(try self.gen_name("tmp"))
                                );
                            } else if (ty.is_unsigned_int()) {
                                return llvm.Core.LLVMBuildURem(
                                    self.llvm_context.builder,
                                    lhs,
                                    rhs,
                                    @ptrCast(try self.gen_name("tmp"))
                                );
                            }
                        },
                        else => unreachable
                    }

                },
                .unary_expr => |expr| {
                    const ex = try self.lower_local(expr.expr);
                    const zero = llvm.Core.LLVMConstInt(llvm_ty, 0, 0);
                    return switch (expr.op) {
                        .Minus => llvm.Core.LLVMBuildNeg(
                            self.llvm_context.builder,
                            ex,
                            @ptrCast(try self.gen_name("tmp"))
                        ),
                        .BinNot => llvm.Core.LLVMBuildNot(
                            self.llvm_context.builder,
                            ex,
                            @ptrCast(try self.gen_name("tmp"))
                        ),
                        .Not => llvm.Core.LLVMBuildICmp(
                            self.llvm_context.builder,
                            llvm.Core.LLVMIntEQ,
                            ex,
                            zero,
                            @ptrCast(try self.gen_name("tmp"))
                        ),
                        else => unreachable
                    };
                },
                .block => |expr| {
                    const prev_scope_id = self.current_scope_id;
                    self.current_scope_id = expr.scope;
                    for (expr.body) |body_node| {
                        _ = try self.lower_local(body_node);
                    }
                    self.current_scope_id = prev_scope_id;
                    return null;
                },
                .fn_call => |call| {
                    const fn_value = try self.lower_local(call.expr);
                    const expr_info = self.hir_info_table.get(call.expr.id).?;
                    const fn_type = self.context.type_tab.get(expr_info.ty.?).?;
                    const llvm_fn_type = try self.createTypeRef(fn_type);
                    var args = try std.ArrayList(llvm.Core.LLVMValueRef)
                        .initCapacity(self.allocator, call.arguments.len);
                    for (call.arguments) |arg| {
                        try args.append(try self.lower_local(arg));
                    }
                    const name = try self.gen_name("tmp");
                    const llvm_call = llvm.Core.LLVMBuildCall2(
                        self.llvm_context.builder,
                        llvm_fn_type,
                        fn_value,
                        (try args.toOwnedSlice()).ptr,
                        @intCast(args.items.len),
                        name
                    );
                    return llvm_call;
                },
                .cast => |cast| {
                    _ = cast;
                },
                .enum_cons => |expr| {
                    _ = expr;
                },
                .struct_cons => |expr| {
                    _ = expr;
                },
                //else => unreachable,
            }
        },
    }
    return null;

}


