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
    node_map: std.AutoHashMap(Hir.HirId, llvm.Core.LLVMValueRef),
},
hir_info_table: *Hir.HirInfoTable,
llvm_id_table: std.AutoHashMap(types.DefId, llvm.Core.LLVMValueRef),
name_index: usize = 0,
current_scope_id: usize = 0,
at_global: bool = true,
destroy_table: *std.AutoHashMap(Hir.HirId, Hir.HirId),
destroy_stub: llvm.Core.LLVMValueRef,

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


pub fn init(
    context: *types.Context, 
    hir_info: *Hir.HirInfoTable, 
    triple: [*]const u8, 
    destroy_tbl: *std.AutoHashMap(Hir.HirId, Hir.HirId),
    allocator: std.mem.Allocator) !@This() {
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

    const llvmcontext = llvm.Core.LLVMContextCreate();
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

    const destroy_ty = llvm.Core.LLVMFunctionType(llvm.Core.LLVMVoidType(), null, 0, 1);
    const destroy_func = llvm.Core.LLVMAddFunction(
        module,
        "destroy",
        destroy_ty
    );
    const bb = llvm.Core.LLVMAppendBasicBlock(
        destroy_func,
        "entry"
    );
    llvm.Core.LLVMPositionBuilderAtEnd(
        builder,
        bb
    );
    _ = llvm.Core.LLVMBuildRetVoid(builder);
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
            .node_map = .init(allocator),
        },
        .context = context,
        .allocator = allocator,
        .hir_info_table = hir_info,
        .llvm_id_table = .init(allocator),
        .destroy_stub = destroy_func,
        .destroy_table = destroy_tbl,
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
    for (hir) |node| {
        const val = try self.lower_local(node);
        if (val != null) {
            try self.llvm_context.node_map.putNoClobber(node.id, val);
        }
    }

    llvm.Core.LLVMDumpModule(self.llvm_context.module);
    var err: [*c]u8 = (try self.allocator.alloc(u8, 5000)).ptr;
    const target_machine = self.llvm_context.target_machine;
    const module = self.llvm_context.module;
    //const file_name = try to_zstring(std.fs.path.basename(self.context.file_path), self.allocator);
    const object_file = llvm.TargetMachine.LLVMObjectFile;
    _ = llvm.TargetMachine.LLVMTargetMachineEmitToFile(
        target_machine, 
        @ptrCast(module), 
        "test.o",
        object_file, 
        &err
    );
}

fn createTypeRef(self: *@This(), ty: Ast.Type,) !llvm.Core.LLVMTypeRef {
    var base_type: llvm.Core.LLVMTypeRef = null;
    switch (ty.base_type) {
        .primitive => |prim| {
            switch (prim) {
                .I8, .U8, .Rune =>  base_type = llvm.Core.LLVMInt8TypeInContext(self.llvm_context.context),
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
        .user => |usr| {
            _ = usr;
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
                        if (decl.is_public) {
                            llvm.Core.LLVMSetLinkage(
                                llvm_func_ref,
                                llvm.Core.LLVMExternalLinkage
                            );
                        }
                        try self.add_llvm_symbol(decl.id, llvm_func_ref);
                        for (decl.parameters, 0..) |param, i| {
                            const path = types.Path {
                                .base = .{ .value = param.id.value, .span = param.id.location },
                                .module = self.context.module.?,
                            };
                            try self.add_llvm_symbol(
                                path.hash(),
                                llvm.Core.LLVMGetParam(llvm_func_ref, @intCast(i))
                            );
                        }
                    },
                    .type_decl => |_| {
                        //for some reason this is required for .func to run?
                        //also this is not really needed here (and shouldn't actually be here)
                        //but since we are lowering directly from HIR its required
                    },
                    .binding => |bind| {
                        const llvm_bind_ty = try self.createTypeRef(self.context.type_tab.get(bind.ty.?).?);
                        const global_ref = llvm.Core.LLVMAddGlobal(
                            self.llvm_context.module,
                            llvm_bind_ty,
                            try self.gen_name("sttc")
                        );
                        try self.add_llvm_symbol(bind.id, global_ref);
                        const initializer_ref = try self.lower_local(bind.expr);
                        if (bind.is_static) {
                            llvm.Core.LLVMSetLinkage(global_ref, llvm.Core.LLVMInternalLinkage);
                        }
                        if (!bind.is_mutable) {
                            llvm.Core.LLVMSetGlobalConstant(global_ref, 1);
                        }
                        llvm.Core.LLVMSetInitializer(global_ref, initializer_ref);
                    },
                    else => {
                        return;
                    }
                }
            },
            else => {
                return;
            }
        }
    }
}


fn lower_local(self: *@This(), node: Hir.Hir) !llvm.Core.LLVMValueRef {
    const hir_info = self.hir_info_table.get(node.id).?;
    const ty = self.context.type_tab.get(hir_info.ty.?).?;
    const llvm_ty = try self.createTypeRef(ty);
    var out_ref: llvm.Core.LLVMValueRef = undefined;
    if (self.destroy_table.get(node.id)) |deid| {
        const deid_ref = self.llvm_context.node_map.get(deid).?;
        var param: [1]llvm.Core.LLVMValueRef = .{ deid_ref };
        _ = llvm.Core.LLVMBuildCall2(
            self.llvm_context.builder,
            llvm.Core.LLVMVoidType(),
            self.destroy_stub,
            &param,
            1,
            try self.gen_name("dest"),
        );
    }
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
                            try self.gen_name("entry"),
                        );
                        llvm.Core.LLVMPositionBuilderAtEnd(
                            self.llvm_context.builder,
                            entry
                        );
                        const out = try self.lower_local(body);
                        self.current_scope_id = saved_scope;
                        out_ref =  out;
                    } else {
                        out_ref = llvm_id;
                    }

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
                        out_ref = store;
                    } else {
                        const val_ref = try self.lower_local(bind.expr);
                        try self.add_llvm_symbol(bind.id, val_ref);
                        out_ref = val_ref;
                    }

                },
                .terminated => |expr| {
                    out_ref = try self.lower_local(expr.*);
                },
                .return_stmt => |stmt| {
                    const ret_expr = try self.lower_local(stmt.expr);
                    out_ref = llvm.Core.LLVMBuildRet(self.llvm_context.builder, ret_expr);
                },
                .assignment => |stmt| {
                    const lvalue_ref = try self.lower_local(stmt.lvalue);
                    const expr_ref = try self.lower_local(stmt.expr);
                    return llvm.Core.LLVMBuildStore(
                        self.llvm_context.builder,
                        expr_ref,
                        lvalue_ref
                    );
                },
                .branch => |stmt| {
                    const cond_ref = try self.lower_local(stmt.condition);
                    const a_block = llvm.Core.LLVMAppendBasicBlockInContext(
                        self.llvm_context.context,
                        self.llvm_context.current_function,
                        try self.gen_name("acond")
                    );
                    llvm.Core.LLVMPositionBuilderAtEnd(
                        self.llvm_context.builder,
                        a_block
                    );

                    _ = try self.lower_local(stmt.a_path);

                    const b_block = llvm.Core.LLVMAppendBasicBlockInContext(
                        self.llvm_context.context,
                        self.llvm_context.current_function,
                        try self.gen_name("bcond")
                    );

                    const after_block = llvm.Core.LLVMAppendBasicBlockInContext(
                        self.llvm_context.context,
                        self.llvm_context.current_function,
                        try self.gen_name("continue")
                    );
                    if (stmt.b_path) |b_path| {
                        llvm.Core.LLVMPositionBuilderAtEnd(
                            self.llvm_context.builder,
                            b_block
                        );
                        _ = try self.lower_local(b_path);
                    }

                    const out = llvm.Core.LLVMBuildCondBr(
                        self.llvm_context.builder,
                        cond_ref, 
                        a_block, 
                        b_block
                    );
                    llvm.Core.LLVMPositionBuilderAtEnd(
                        self.llvm_context.builder,
                        after_block
                    );
                    return out;
                },
                .loop => |stmt| {
                    _ = stmt;
                },
                .type_decl => |decl| {
                    _ = decl;
                },
            }
        },
        .inline_expr => {
            switch (node.node.inline_expr) {
                .terminal => |term| {
                    switch (term.*) {
                        .integer_literal => |val| {
                            out_ref = llvm.Core.LLVMConstInt(llvm_ty, @intCast(val), 0);
                        },
                        .float_literal => |val| {
                            out_ref = llvm.Core.LLVMConstReal(llvm_ty, val);
                        },
                        .char_literal => |val| {
                            out_ref = llvm.Core.LLVMConstInt(llvm_ty, @intCast(val), 0);
                        },
                        .string_literal => |val| {
                            out_ref = llvm.Core.LLVMBuildGlobalStringPtr(
                                self.llvm_context.builder,
                                try to_zstring(val, self.allocator),
                                try self.gen_name("gblstr"),
                            );
                        },
                        .bool_literal => |val| {
                            out_ref = llvm.Core.LLVMConstInt(llvm_ty, if (val) 1 else 0, 0);
                        },
                        .path => |val| {
                            const sym = self.get_symbol(val).?;
                            _ = sym;
                            out_ref = self.get_llvm_symbol(val);
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
                        .Add => out_ref = llvm.Core.LLVMBuildAdd(
                            self.llvm_context.builder, 
                            lhs, 
                            rhs, 
                            @ptrCast(try self.gen_name("tmp"))
                        ),
                        .Sub => out_ref =  llvm.Core.LLVMBuildSub(
                            self.llvm_context.builder, 
                            lhs, 
                            rhs, 
                            @ptrCast(try self.gen_name("tmp"))
                        ),
                        .Mul => out_ref = llvm.Core.LLVMBuildMul(
                            self.llvm_context.builder, 
                            lhs, 
                            rhs, @ptrCast(try self.gen_name("tmp"))
                        ),
                        .Div => {
                            if (ty.is_signed_int()) {
                                out_ref = llvm.Core.LLVMBuildSDiv(
                                    self.llvm_context.builder, 
                                    lhs, 
                                    rhs, 
                                    @ptrCast(try self.gen_name("tmp"))
                                );
                            } else if (ty.is_unsigned_int()) {
                                out_ref = llvm.Core.LLVMBuildUDiv(
                                    self.llvm_context.builder, 
                                    lhs, 
                                    rhs, 
                                    @ptrCast(try self.gen_name("tmp"))
                                );
                            }
                        },
                        .Mod => {
                            if (ty.is_signed_int()) {
                                out_ref = llvm.Core.LLVMBuildSRem(
                                    self.llvm_context.builder, 
                                    lhs, 
                                    rhs, @ptrCast(try self.gen_name("tmp"))
                                );
                            } else if (ty.is_unsigned_int()) {
                                out_ref = llvm.Core.LLVMBuildURem(
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
                    out_ref = switch (expr.op) {
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
                        .Ref => ex,
                        else => unreachable,

                    };
                },
                .block => |expr| {
                    const prev_scope_id = self.current_scope_id;
                    const prev_at_global = self.at_global;
                    self.at_global = false;
                    self.current_scope_id = expr.scope;
                    for (expr.body) |body_node| {
                        _ = try self.lower_local(body_node);
                    }
                    self.current_scope_id = prev_scope_id;
                    self.at_global = prev_at_global;
                    out_ref = null;
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
                    const name = try self.gen_name("tcll");
                    const args_slice = try args.toOwnedSlice();

                    const llvm_call = llvm.Core.LLVMBuildCall2(
                        self.llvm_context.builder,
                        llvm_fn_type,
                        fn_value,
                        args_slice.ptr,
                        @intCast(args_slice.len),
                        name
                    );
                    out_ref = llvm_call;
                },
                .cast => |cast| {
                    var expr_value = try self.lower_local(cast.expr);
                    const cast_info = self.hir_info_table.get(cast.expr.id).?;
                    const old_ty = self.context.type_tab.get(cast.tyid).?;
                    const new_ty = self.context.type_tab.get(cast_info.ty.?).?;
                    var opcode: c_uint = llvm.Core.LLVMBitCast;
                    if (old_ty.modifiers != null and old_ty.modifiers.?.len > 0
                        and old_ty.modifiers.?[0] == .Ref 
                        and (new_ty.modifiers == null or new_ty.modifiers.?.len == 0) 
                        and new_ty.is_int()) {
                        opcode = llvm.Core.LLVMPtrToInt;
                    } else {
                        if ( (old_ty.is_int() or old_ty.is_float()) and ( new_ty.is_int() or new_ty.is_float())) {
                            opcode = self.get_castcode(cast.tyid, cast_info.ty.?);
                        }
                    }
                    const new_llvm = try self.createTypeRef(new_ty);
                    expr_value = llvm.Core.LLVMBuildCast(
                        self.llvm_context.builder,
                        opcode,
                        expr_value,
                        new_llvm,
                        try self.gen_name("cast"),
                    );
                    out_ref = expr_value;

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
    if (try self.apply_adjustments(node, out_ref)) |new_ref| {
        out_ref = new_ref;
    }
    return out_ref;

}


fn apply_adjustments(self: *@This(), node: Hir.Hir, value: llvm.Core.LLVMValueRef) !?llvm.Core.LLVMValueRef {
    const hir_info = self.hir_info_table.get(node.id).?;
    const hir_ty = self.context.type_tab.get(hir_info.ty.?).?;
    const hir_llvm_ty = try self.createTypeRef(hir_ty);
    if (hir_info.adjustments == null or hir_info.adjustments.?.len == 0) return null;
    var out_value = value;
    for (hir_info.adjustments.?) |adjustment| {
        switch (adjustment) {
            .NumericCast => |cast| {
                const new_llvm = try self.createTypeRef(self.context.type_tab.get(cast).?);
                const opcode = self.get_castcode(hir_info.ty.?, cast);
                out_value = llvm.Core.LLVMBuildCast(
                    self.llvm_context.builder,
                    opcode,
                    value,
                    new_llvm,
                    try self.gen_name("numcst"),
                );
            },
            .AutoDeref => {
                out_value = llvm.Core.LLVMBuildLoad2(
                    self.llvm_context.builder,
                    hir_llvm_ty,
                    value,
                    try self.gen_name("audrf")
                );

            },
            .PointerCast => |tyid| {
                const new_llvm = try self.createTypeRef(self.context.type_tab.get(tyid).?);
                out_value = llvm.Core.LLVMBuildCast(
                    self.llvm_context.builder,
                    llvm.Core.LLVMBitCast,
                    value,
                    new_llvm,
                    try self.gen_name("ptrcst"),
                );
            },
            .MutDiscard => {},
            .RefMutDiscard => {},
        }
    }
    return out_value;
}

fn get_castcode(self: *@This(), src: Ast.TypeId, dest: Ast.TypeId) llvm.Core.LLVMOpcode {
    const src_ty = self.context.type_tab.get(src).?;
    const dest_ty = self.context.type_tab.get(dest).?;
    const src_size = src_ty.get_size(self.llvm_context.target_size);
    const dest_size = dest_ty.get_size(self.llvm_context.target_size);
    if (src_ty.is_int() and dest_ty.is_int()) {
        if (src_ty.is_signed_int() != dest_ty.is_signed_int()
            and src_size == dest_size) {
            return llvm.Core.LLVMBitCast;
        }
        if (src_size < dest_size) {
            if (src_ty.is_signed_int()) {
                return llvm.Core.LLVMSExt;
            }
            return llvm.Core.LLVMZExt;
        }
        if (src_size > dest_size) {
            return llvm.Core.LLVMTrunc;
        }
    }
    if (src_ty.is_int() and dest_ty.is_float()) {
        if (src_ty.is_signed_int()) {
            return llvm.Core.LLVMSIToFP;
        }
        return llvm.Core.LLVMUIToFP;
    }

    if (src_ty.is_float() and dest_ty.is_int()) {
        if (dest_ty.is_signed_int()) {
            return llvm.Core.LLVMFPToSI;
        }
        return llvm.Core.LLVMFPToUI;
    }

    if (src_ty.is_float() and dest_ty.is_float()) {
        if (src_size < dest_size) {
            return llvm.Core.LLVMFPExt;
        }
        if (src_size > dest_size) {
            return llvm.Core.LLVMFPTrunc;
        }
    }
    return llvm.Core.LLVMBitCast;
}
