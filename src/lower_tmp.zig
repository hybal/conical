const std = @import("std");

const llvm= @import("llvm");

const Ast = @import("Ast.zig");
const types = @import("types.zig");

pub const LLVMContext = struct {

    builder: llvm.Core.LLVMBuilderRef,
    context: llvm.Core.LLVMContextRef,
    target_machine: llvm.TargetMachine.LLVMTargetMachineRef,
    target_data: llvm.Target.LLVMTargetDataRef, 
    data_layout: llvm.TargetMachine.LLVMTargetDataRef,
    module: llvm.Core.LLVMModuleRef,
};
llvm_context: LLVMContext,
name_index: usize,
allocator: std.mem.Allocator,
comp_unit: types.CompUnit,
symbol_map: std.StringHashMap(struct {value: llvm.Core.LLVMValueRef, ty: llvm.Core.LLVMTypeRef, is_alloca: bool}),

pub fn gen_name(self: *@This(), prefix: []const u8) ![]const u8 {
    const name = try std.fmt.allocPrint(self.allocator, "{s}{d}{c}", .{prefix, self.name_index, 0});
    self.name_index += 1;
    return name;
}

pub fn init(comp_unit: types.CompUnit, triple: [*]const u8, allocator: std.mem.Allocator) !@This() {
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
        llvm.TargetMachine.LLVMRelocDefault,
        llvm.TargetMachine.LLVMCodeModelDefault,
    );

    const context = llvm.Core.LLVMContextCreate();
    const builder = llvm.Core.LLVMCreateBuilderInContext(context);
    const data_layout = llvm.TargetMachine.LLVMCreateTargetDataLayout(target_machine);
    const layout_str = llvm.TargetMachine.LLVMCopyStringRepOfTargetData(data_layout);
    defer llvm.Core.LLVMDisposeMessage(layout_str);
    const module = llvm.Core.LLVMModuleCreateWithNameInContext("main", context);
    const target_data = llvm.Target.LLVMCreateTargetData(layout_str);
    return .{
        .llvm_context = .{
            .builder = builder,
            .context = context,
            .target_machine = target_machine,
            .target_data = target_data,
            .data_layout = data_layout,
            .module = module,
        },
        .name_index = 0,
        .allocator = allocator,
        .comp_unit = comp_unit,
        .symbol_map = .init(allocator),
    };
}
pub fn createTypeRef(self: *@This(), ty: Ast.Type,) !llvm.Core.LLVMTypeRef {
    var base_type: llvm.Core.LLVMTypeRef = null;
    switch (ty.base_type) {
        .primitive => |prim| {
            switch (prim) {
                .I8, .U8 =>  base_type = llvm.Core.LLVMInt8TypeInContext(self.llvm_context.context),
                .I16, .U16 => base_type = llvm.Core.LLVMInt16TypeInContext(self.llvm_context.context),
                .I32, .U32, .Rune => base_type = llvm.Core.LLVMInt32TypeInContext(self.llvm_context.context),
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
                const llvm_type = try self.createTypeRef(self.comp_unit.type_table.get(next.*).?);
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
                    const nxt_ty = self.comp_unit.type_table.get(nxt).?;
                    const llvm_ty = try self.createTypeRef(nxt_ty);
                    const size = llvm.Target.LLVMABISizeOfType(self.llvm_context.target_data, @ptrCast(llvm_ty));
                    if (size > max_size) max_size = size;

                }
            }
            const storage_ty = llvm.Core.LLVMArrayType(try self.createTypeRef(Ast.Type.createPrimitive(.I8, null)), @intCast(max_size));
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
                const arg = self.comp_unit.type_table.get(argid).?;
                try fnc_args.append(try self.createTypeRef(arg));
            }
            const fnc_args_slice = try fnc_args.toOwnedSlice();
            const ret_ty = try self.createTypeRef(self.comp_unit.type_table.get(fnc.ret).?);
            base_type = llvm.Core.LLVMFunctionType(ret_ty, @ptrCast(fnc_args_slice), @intCast(fnc_args_slice.len), 0);
        },
        else => unreachable,
    }
    return base_type;
}

fn parse_int_literal(self: *@This(), span: types.Span) !c_ulonglong {
    const literal_string = span.get_string(self.comp_unit.source);
    return std.fmt.parseInt(c_ulonglong, literal_string, 0);
}

fn parse_float_literal(self: *@This(), span: types.Span) !f64 {
    const literal_string = span.get_string(self.comp_unit.source);
    return std.fmt.parseFloat(f64, literal_string);
}

fn parse_escape(str: []const u8) !u32 {
    switch (str[0]) {
        'n' => return '\n',
        'r' => return '\r',
        't' => return '\t',
        'b' => return 0x8,
        'f' => return 0xC,
        'v' => return 0xB,
        '\'' => return '\'',
        '\"' => return '\"',
        '0' => return 0x0,
        'x', 'u', 'U' => {
            const num = str[1..];
            return std.fmt.parseInt(u32, num, 16);
        },
        else => {
            return error.InvalidEscapeSequence;
        }
    }
}

fn parse_char_literal(self: *@This(), span: types.Span) !u32 {
    var local_span: types.Span = .{.start = span.start, .end = span.end};
    local_span.start += 1;
    local_span.end -= 1;
    const char_string = local_span.get_string(self.comp_unit.source);
    if (char_string[0] == '\\') {
        return parse_escape(char_string[0..]);
    }
    return char_string[0];
    
}

pub fn emit(self: *@This()) void {
    llvm.Core.LLVMDumpModule(self.llvm_context.module);
}
pub fn lower(self: *@This(), ast: *Ast.Ast) !llvm.Core.LLVMValueRef {
    const node_type = self.comp_unit.type_table.get(ast.tyid.?).?;
    const type_ref = try self.createTypeRef(node_type);
    switch (ast.node) {
        .terminal => |term| {
            switch (term.tag) {
                .int_literal => {
                    return llvm.Core.LLVMConstInt(type_ref, try self.parse_int_literal(term.span), 0);
                },
                .float_literal => {
                    return llvm.Core.LLVMConstReal(type_ref, try self.parse_float_literal(term.span));
                },
                .string_literal, .raw_string_literal => {
                    const actual_span: types.Span = .{
                        .start = term.span.start + 1,
                        .end = term.span.end - 1
                    };
                    return llvm.Core.LLVMConstStringInContext(self.llvm_context.context, 
                        @ptrCast(actual_span.get_string(self.comp_unit.source)),
                        @intCast(actual_span.end - actual_span.start),
                        1
                    );
                },
                .char_literal => {
                    return llvm.Core.LLVMConstInt(type_ref, try self.parse_char_literal(term.span), 0);
                },
                .ident => {
                    const symref = self.symbol_map.get(term.span.get_string(self.comp_unit.source)).?;
                    if (symref.is_alloca) {
                        const id = try self.allocator.dupeZ(u8,term.span.get_string(self.comp_unit.source));
                        const loaded = llvm.Core.LLVMBuildLoad2(self.llvm_context.builder, symref.ty, symref.value, @ptrCast(id));
                        return loaded;
                    }
                    return symref.value;
                },
                else => |val| {
                    std.debug.print("Reached unhandled switch case: {s}\n", .{@tagName(val)});
                    unreachable;
                },
            }
        },
        .binary_expr => |expr| {
            const lhs = try self.lower(expr.left);
            const rhs = try self.lower(expr.right);
            switch (expr.op.tag) {
                .plus => return llvm.Core.LLVMBuildAdd(self.llvm_context.builder, lhs, rhs, @ptrCast(try self.gen_name("tmp"))),
                .minus => return llvm.Core.LLVMBuildSub(self.llvm_context.builder, lhs, rhs, @ptrCast(try self.gen_name("tmp"))),
                .star => return llvm.Core.LLVMBuildMul(self.llvm_context.builder, lhs, rhs, @ptrCast(try self.gen_name("tmp"))),
                .slash => {
                    if (node_type.is_signed_int()) {
                        return llvm.Core.LLVMBuildSDiv(self.llvm_context.builder, lhs, rhs, @ptrCast(try self.gen_name("tmp")));
                    } else if (node_type.is_unsigned_int()) {
                        return llvm.Core.LLVMBuildUDiv(self.llvm_context.builder, lhs, rhs, @ptrCast(try self.gen_name("tmp")));
                    }
                },
                .percent => {
                    if (node_type.is_signed_int()) {
                        return llvm.Core.LLVMBuildSRem(self.llvm_context.builder, lhs, rhs, @ptrCast(try self.gen_name("tmp")));
                    } else if (node_type.is_unsigned_int()) {
                        return llvm.Core.LLVMBuildURem(self.llvm_context.builder, lhs, rhs, @ptrCast(try self.gen_name("tmp")));
                    }
                },
                else => unreachable
            }
        },
        .unary_expr => |expr| {
            const ex = try self.lower(expr.expr);
            const ty = try self.createTypeRef(self.comp_unit.type_table.get(ast.tyid.?).?);
            const zero = llvm.Core.LLVMConstInt(ty, 0, 0);
            switch (expr.op.tag) {
                .minus => llvm.Core.LLVMBuildNeg(self.llvm_context.builder, ex, @ptrCast(try self.gen_name("tmp"))),
                .tilde => llvm.Core.LLVMBuildNot(self.llvm_context.builder, ex, @ptrCast(try self.gen_name("tmp"))),
                .bang => llvm.Core.LLVMBuildICmp(self.llvm_context.builder, llvm.Core.LLVMIntEq, ex, zero, @ptrCast(self.gen_name("tmp"))),
                else => unreachable
            }
        },
        .fn_decl => |decl| {
            const fnc_ty: Ast.Type = .{
                .base_type = .{ 
                    .func = .{
                        .args = decl.param_types,
                        .ret = decl.return_ty
                    }
                },
                .modifiers = null,
            };

            const llvm_fnc_ty = try self.createTypeRef(fnc_ty);
            const id = try self.allocator.dupeZ(u8, decl.ident.value);
            const func_ref = llvm.Core.LLVMAddFunction(self.llvm_context.module, @ptrCast(id), llvm_fnc_ty);
            const entry = llvm.Core.LLVMAppendBasicBlockInContext(self.llvm_context.context, func_ref, "entry");
            llvm.Core.LLVMPositionBuilderAtEnd(self.llvm_context.builder, entry);
            return try self.lower(decl.body.?);
        },
        .block => |block| {
            for (block.exprs) |expr| {
                _ = try self.lower(expr);
            }
            return null;
        },
        .return_stmt => |ret| {
            return llvm.Core.LLVMBuildRet(self.llvm_context.builder, try self.lower(ret));
        },
        .var_decl => |decl| {
            const var_ty = try self.createTypeRef(self.comp_unit.type_table.get(decl.ty.?).?);
            const val = try self.lower(decl.initialize.?);
            if (decl.is_mut) {
                const name = try self.gen_name("tmp");
                const value_ref = llvm.Core.LLVMBuildAlloca(self.llvm_context.builder, var_ty, @ptrCast(name));

                _ = try self.symbol_map.getOrPutValue(decl.ident.value, .{.value = val, .ty = var_ty, .is_alloca = true});
                const store = llvm.Core.LLVMBuildStore(self.llvm_context.builder, val, value_ref);
                return store;
            }
            _ = try self.symbol_map.getOrPutValue(decl.ident.value, .{.value = val, .ty = var_ty, .is_alloca = false});
            return val;
        },
        else => |val| {
            std.debug.print("Unhandled switch case: {s}\n", .{@tagName(val)});
            unreachable;
        },
    }
    std.debug.print("Should not reach end of function\n", .{});
    unreachable;
}

