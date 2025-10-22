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

fn to_zstring(string: []const u8, allocator: std.mem.Allocator) ![*:0]const u8 {
    return allocator.dupeZ(u8, string);
}

fn gen_name(self: *@This(), prefix: []const u8) ![*:0]const u8 {
    const name = try std.fmt.allocPrintZ(self.allocator, "{s}{d}", .{prefix, self.name_index});
    self.name_index += 1;
    return name;
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
        target.?,
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
    defer llvm.Core.LLVMDisposeMessage(layout_str);
    const module = llvm.Core.LLVMModuleCreateWithNameInContext("main", llvmcontext);
    const target_data = llvm.Target.LLVMCreateTargetData(layout_str);
    const pm = llvm.Core.LLVMCreateFunctionPassManagerForModule(module);
    llvm.Core.LLVMSetTarget(module, triple);
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

pub fn emit(self: *@This(), hir: []Hir.Hir) !void {
    try self.lower_global(hir);
    llvm.Core.LLVMDumpModule(self.llvm_context.module);
    var err: [*c]u8 = null;
    _ = llvm.TargetMachine.LLVMTargetMachineEmitToFile(
        self.llvm_context.target_machine, 
        @ptrCast(self.llvm_context.module), 
        @ptrCast(try self.allocator.dupeZ(u8, std.fs.path.basename(self.context.file_path))), 
        llvm.TargetMachine.LLVMObjectFile, 
        &err
    );
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
            var fnc_args = std.ArrayList(llvm.Core.LLVMTypeRef).init(self.allocator);
            for (fnc.args) |argid| {
                const arg = self.context.type_tab.get(argid).?;
                try fnc_args.append(try self.createTypeRef(arg));
            }
            const fnc_args_slice = try fnc_args.toOwnedSlice();
            const ret_ty = try self.createTypeRef(self.context.type_tab.get(fnc.ret).?);
            base_type = llvm.Core.LLVMFunctionType(ret_ty, @ptrCast(fnc_args_slice), @intCast(fnc_args_slice.len), 0);
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
                            @constCast(&[_]llvm.Core.LLVMTypeRef{ ptr_type, llvm.Core.LLVMIntType(self.target_size) }),
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
                        _ = decl;
                    }
                }
            },
        }
    }
}

fn lower_local(self: *@This(), node: Hir.Hir) !llvm.Core.LLVMValueRef {
    const hir_info = self.hir_info_table.get(node.id).?;
    const ty = self.context.type_tab.get(hir_info.ty.?).?;
    const llvm_ty = try self.adjust(node, try self.createTypeRef(ty));
    switch (node.node) {
        .top_level => {},
        .inline_expr => {
            switch (node.node.inline_expr) {
                .terminal => |term| {
                    switch (term.*) {
                        .integer_literal => |val| {
                            return llvm.Core.LLVMConstInt(llvm_ty, val, 0);
                        },
                        .float_literal => |val| {
                            return llvm.Core.LLVMConstReal(llvm_ty, val);
                        },
                        .char_literal => |val| {
                            return llvm.Core.LLVMConstInt(llvm_ty, val, 0);
                        },
                        .string_literal => |val| {
                            return llvm.Core.LLVMBuildGlobalStringPtr(
                                self.llvm_context.builder,
                                to_zstring(val, self.allocator),
                                to_zstring(try self.gen_name("gblstr")),
                            );
                        },
                        .bool_literal => |val| {
                            return llvm.Core.LLVMConstInt(llvm_ty, if (val) 1 else 0, 0);
                        },
                        else => {
                            std.debug.print("Unhandled switch case at lower_local.inline_expr.terminal\n", .{});
                            unreachable;
                        }
                    }
                },
                .binary_expr => |expr| {
                    const lhs = try self.lower_local(expr.left);
                    const rhs = try self.lower_local(expr.right);
                    switch (expr.op.tag) {
                        .plus => return llvm.Core.LLVMBuildAdd(
                            self.llvm_context.builder, 
                            lhs, 
                            rhs, 
                            @ptrCast(try self.gen_name("tmp"))
                        ),
                        .minus => return llvm.Core.LLVMBuildSub(
                            self.llvm_context.builder, 
                            lhs, 
                            rhs, 
                            @ptrCast(try self.gen_name("tmp"))
                        ),
                        .star => return llvm.Core.LLVMBuildMul(
                            self.llvm_context.builder, 
                            lhs, 
                            rhs, @ptrCast(try self.gen_name("tmp"))
                        ),
                        .slash => {
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
                        .percent => {
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
                    const zero = llvm.Core.LLVMConstInt(ty, 0, 0);
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
                    _ = call;
                },
                else => unreachable,
            }
        },
    }

}


