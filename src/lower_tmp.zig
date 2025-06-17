const std = @import("std");

const llvm= @import("llvm");

const Ast = @import("Ast.zig");
const types = @import("types.zig");

const SymbolEntry = struct {
    value: llvm.Core.LLVMValueRef,
    ty: llvm.Core.LLVMTypeRef,
    is_alloca: bool
};

pub const LLVMContext = struct {

    builder: llvm.Core.LLVMBuilderRef,
    context: llvm.Core.LLVMContextRef,
    target_machine: llvm.TargetMachine.LLVMTargetMachineRef,
    target_data: llvm.Target.LLVMTargetDataRef, 
    data_layout: llvm.TargetMachine.LLVMTargetDataRef,
    module: llvm.Core.LLVMModuleRef,
    current_function: llvm.Core.LLVMValueRef = null,
    pm: llvm.Core.LLVMPassManagerRef,
};
llvm_context: LLVMContext,
name_index: usize,
allocator: std.mem.Allocator,
comp_unit: types.CompUnit,
symbol_map: std.ArrayList(std.StringHashMap(SymbolEntry)),
in_assignment: bool,
target_size: u8,
at_global: bool = true,

fn get_symbol(self: *@This(), sym: []const u8) ?SymbolEntry {
    var i: usize = self.symbol_map.items.len - 1;
    while (i >= 0) : ( i -= 1 ) {
        if (self.symbol_map.items[i].get(sym)) |out| {
            return out;
        }
    }
    return null;
}

fn put_symbol(self: *@This(), sym: []const u8, value: SymbolEntry) !void {
    _ = try self.symbol_map.items[self.symbol_map.items.len - 1].getOrPutValue(sym, value);
}

fn create_scope(self: *@This()) !void {
    try self.symbol_map.append(.init(self.allocator));
}
fn exit_scope(self: *@This()) void {
    self.symbol_map.shrinkRetainingCapacity(self.symbol_map.items.len - 1);
}
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
        llvm.TargetMachine.LLVMRelocPIC,
        llvm.TargetMachine.LLVMCodeModelDefault,
    );

    const context = llvm.Core.LLVMContextCreate();
    const builder = llvm.Core.LLVMCreateBuilderInContext(context);
    const data_layout = llvm.TargetMachine.LLVMCreateTargetDataLayout(target_machine);
    const layout_str = llvm.TargetMachine.LLVMCopyStringRepOfTargetData(data_layout);
    defer llvm.Core.LLVMDisposeMessage(layout_str);
    const module = llvm.Core.LLVMModuleCreateWithNameInContext("main", context);
    const target_data = llvm.Target.LLVMCreateTargetData(layout_str);
    const pm = llvm.Core.LLVMCreateFunctionPassManagerForModule(module);
    llvm.Core.LLVMSetTarget(module, triple);
    var arr_map: std.ArrayList(std.StringHashMap(SymbolEntry)) = .init(allocator);
    try arr_map.append(.init(allocator));
    return .{
        .llvm_context = .{
            .builder = builder,
            .context = context,
            .target_machine = target_machine,
            .target_data = target_data,
            .data_layout = data_layout,
            .module = module,
            .pm = pm
        },
        .name_index = 0,
        .allocator = allocator,
        .comp_unit = comp_unit,
        .symbol_map = arr_map,
        .in_assignment = false,
        .target_size = @intCast(llvm.Target.LLVMPointerSize(target_data) * 8)
    };
}
pub fn createTypeRef(self: *@This(), ty: Ast.Type,) !llvm.Core.LLVMTypeRef {
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
fn parseHexEscape(comptime T: type, slice: []const u8) !T {
    return std.fmt.parseInt(T, slice, 16) catch return error.InvalidHexEscape;
}
fn unescape_string(self: *@This(), input: []const u8) ![]u8 {
    var list = std.ArrayList(u8).init(self.allocator);
    defer list.deinit();

    var i: usize = 0;
    while (i < input.len) : (i += 1){
        if (input[i] == '\\' and i + 1 < input.len) {
            i += 1;
            switch (input[i]) {
                'n' => try list.append('\n'),
                'r' => try list.append('\r'),
                't' => try list.append('\t'),
                'b' => try list.append(0x8),
                'f' => try list.append(0xC),
                'v' => try list.append(0xB),
                '\\' => try list.append('\\'),
                '"' => try list.append('"'),
                '\'' => try list.append('\''),
                '0' => try list.append(0),
                'x' => {
                    // \xHH (2 hex digits)
                    if (i + 2 >= input.len) return error.InvalidHexEscape;
                    const hex = input[i+1 .. i+3];
                    const value = try parseHexEscape(u8, hex);
                    try list.append(value);
                    i += 2;
                },

                'u' => {
                    // \uHHHH (4 hex digits)
                    if (i + 4 >= input.len) return error.InvalidUnicodeEscape;
                    const hex = input[i+1 .. i+5];
                    const codepoint = try parseHexEscape(u21, hex);
                    var buf: [4]u8 = undefined;
                    const len = std.unicode.utf8Encode(codepoint, &buf) catch return error.InvalidCodepoint;
                    try list.appendSlice(buf[0..len]);
                    i += 4;
                },

                'U' => {
                    // \UHHHHHHHH (8 hex digits)
                    if (i + 8 >= input.len) return error.InvalidUnicodeEscape;
                    const hex = input[i+1 .. i+9];
                    const codepoint = try parseHexEscape(u21, hex);
                    var buf: [4]u8 = undefined;
                    const len = std.unicode.utf8Encode(codepoint, &buf) catch return error.InvalidCodepoint;
                    try list.appendSlice(buf[0..len]);
                    i += 8;
                },                
                else => unreachable,
            }
        } else {
            try list.append(input[i]);
        }
    }

    return list.toOwnedSlice();
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

pub fn emit(self: *@This()) !void {
    llvm.Core.LLVMDumpModule(self.llvm_context.module);
    var err: [*c]u8 = null;
    _ = llvm.TargetMachine.LLVMTargetMachineEmitToFile(self.llvm_context.target_machine, @ptrCast(self.llvm_context.module), @ptrCast(try self.allocator.dupeZ(u8, self.comp_unit.out_file)), llvm.TargetMachine.LLVMObjectFile, &err);
}

pub fn lower_global(self: *@This(), trees: []*Ast.Ast) !void{
    for (trees) |ast| {
        switch (ast.node) {
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
                if (decl.decl_mod != null and decl.decl_mod.? == .Extern) {
                    llvm.Core.LLVMSetLinkage(func_ref, llvm.Core.LLVMExternalLinkage);
                }
                _ = try self.put_symbol(decl.ident.value, .{.value = func_ref, .ty = llvm_fnc_ty, .is_alloca = false});
            },
            else => unreachable
        }
    }
}
pub fn lower(self: *@This(), trees: []*Ast.Ast) !void {
    try self.lower_global(trees);
    self.at_global = false;
    for (trees) |ast| {
        _ = try self.lower_local(ast);
    }
}
fn lower_local(self: *@This(), ast: *Ast.Ast) !llvm.Core.LLVMValueRef {
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
                    var string = actual_span.get_string(self.comp_unit.source);
                    if (term.tag == .string_literal) {
                        string = try self.unescape_string(string);
                    }
                    return llvm.Core.LLVMBuildGlobalStringPtr(self.llvm_context.builder, 
                        @ptrCast(string),
                        @ptrCast(try self.gen_name("tmp")),
                    );
                                    },
                .char_literal => {
                    return llvm.Core.LLVMConstInt(type_ref, try self.parse_char_literal(term.span), 0);
                },
                .ident => {
                    const symref = self.get_symbol(term.span.get_string(self.comp_unit.source)).?;
                    if (symref.is_alloca and !self.in_assignment) {
                        const id = try self.allocator.dupeZ(u8, term.span.get_string(self.comp_unit.source));
                        const ptr_ty = llvm.Core.LLVMPointerType(symref.ty, 0);
                        const loaded = llvm.Core.LLVMBuildLoad2(self.llvm_context.builder, ptr_ty, symref.value, @ptrCast(id));
                        return loaded;
                    }
                    return symref.value;
                },
                .keyword_true => {
                    return llvm.Core.LLVMConstInt(type_ref, 1, 0);
                },
                .keyword_false => {
                    return llvm.Core.LLVMConstInt(type_ref, 0, 0);
                },
                else => |val| {
                    std.debug.print("Reached unhandled switch case: {s}\n", .{@tagName(val)});
                    unreachable;
                },
            }
        },
        .binary_expr => |expr| {
            const lhs = try self.lower_local(expr.left);
            const rhs = try self.lower_local(expr.right);
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
            const ex = try self.lower_local(expr.expr);
            const ty = try self.createTypeRef(self.comp_unit.type_table.get(ast.tyid.?).?);
            const zero = llvm.Core.LLVMConstInt(ty, 0, 0);
            return switch (expr.op.tag) {
                .minus => llvm.Core.LLVMBuildNeg(self.llvm_context.builder, ex, @ptrCast(try self.gen_name("tmp"))),
                .tilde => llvm.Core.LLVMBuildNot(self.llvm_context.builder, ex, @ptrCast(try self.gen_name("tmp"))),
                .bang => llvm.Core.LLVMBuildICmp(self.llvm_context.builder, llvm.Core.LLVMIntEQ, ex, zero, @ptrCast(try self.gen_name("tmp"))),
                else => unreachable
            };
        },
        .fn_decl => |decl| {
            if (!self.at_global) {
                const fnc_ty = self.get_symbol(decl.ident.value).?;
                if (decl.body) |body| {
                    try self.create_scope();
                    for (decl.params, 0..) |param, i| {
                        const param_ty = self.comp_unit.type_table.get(decl.param_types[i]).?;
                        _ = try self.put_symbol(param.value, .{ 
                            .value = llvm.Core.LLVMGetParam(fnc_ty.value, @intCast(i)),
                            .ty = try self.createTypeRef(param_ty),
                            .is_alloca = false
                        });
                    }
                    const entry = llvm.Core.LLVMAppendBasicBlockInContext(self.llvm_context.context, fnc_ty.value, "entry");
                    llvm.Core.LLVMPositionBuilderAtEnd(self.llvm_context.builder, entry);
                    const saved_current_function = self.llvm_context.current_function;
                    self.llvm_context.current_function = fnc_ty.value;
                    const out = try self.lower_local(body);
                    self.llvm_context.current_function = saved_current_function;
                    self.exit_scope();
                    return out;
                }
                return fnc_ty.value;

            }
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
            if (decl.decl_mod != null and decl.decl_mod.? == .Extern) {
                llvm.Core.LLVMSetLinkage(func_ref, llvm.Core.LLVMExternalLinkage);
            }
            _ = try self.put_symbol(decl.ident.value, .{.value = func_ref, .ty = llvm_fnc_ty, .is_alloca = false});
            if (decl.body) |_| {
                const entry = llvm.Core.LLVMAppendBasicBlockInContext(self.llvm_context.context, func_ref, "entry");
                llvm.Core.LLVMPositionBuilderAtEnd(self.llvm_context.builder, entry);
                return try self.lower_local(decl.body.?);
            }
            return func_ref;
        },
        .block => |block| {
            try self.create_scope();
            for (block.exprs) |expr| {
                _ = try self.lower_local(expr);
            }
            self.exit_scope();
            return null;
        },
        .return_stmt => |ret| {
            return llvm.Core.LLVMBuildRet(self.llvm_context.builder, try self.lower_local(ret));
        },
        .var_decl => |decl| {
            const var_ty = try self.createTypeRef(self.comp_unit.type_table.get(decl.ty.?).?);
            const val = try self.lower_local(decl.initialize.?);
            if (decl.is_mut) {
                const name = try self.gen_name("tmp");
                const value_ref = llvm.Core.LLVMBuildAlloca(self.llvm_context.builder, var_ty, @ptrCast(name));

                _ = try self.put_symbol(decl.ident.value, .{.value = value_ref, .ty = var_ty, .is_alloca = true});
                const store = llvm.Core.LLVMBuildStore(self.llvm_context.builder, val, value_ref);
                return store;
            }
            _ = try self.put_symbol(decl.ident.value, .{.value = val, .ty = var_ty, .is_alloca = false});
            return val;
        },
        .assignment => |expr| {
            const saved_in_assignment = self.in_assignment;
            self.in_assignment = true;
            const lhs = try self.lower_local(expr.lvalue);
            self.in_assignment = saved_in_assignment;
            const rhs = try self.lower_local(expr.expr);
            return llvm.Core.LLVMBuildStore(self.llvm_context.builder, rhs, lhs);
        },
        .terminated => |expr| {
            return try self.lower_local(expr);
        },
        .cast => |expr| {
            if (expr.expr.tyid.? == expr.ty) return self.lower_local(expr.expr);
            const from_ty_full = self.comp_unit.type_table.get(expr.expr.tyid.?).?;
            const from_ty = from_ty_full.base_type.primitive;
            const to_ty_full = self.comp_unit.type_table.get(expr.ty).?;
            const to_ty = to_ty_full.base_type.primitive;
            const to_ty_llvm = try self.createTypeRef(to_ty_full);
            const lowered_expr = try self.lower_local(expr.expr);
            const from_is_signed = (from_ty.is_float() or from_ty.is_signed_int()) and from_ty_full.modifiers == null;
            const to_is_signed = (to_ty.is_float() or to_ty.is_signed_int()) and to_ty_full.modifiers == null;
            const cast_op = llvm.Core.LLVMGetCastOpcode(lowered_expr, if (from_is_signed) 1 else 0, to_ty_llvm, if (to_is_signed) 1 else 0); 
            return llvm.Core.LLVMBuildCast(
                self.llvm_context.builder,
                cast_op,
                lowered_expr,
                to_ty_llvm,
                @ptrCast(try self.gen_name("tmp")),
            );

            // if (from_ty.is_int() and to_ty.is_int()) {
            //     const name = try self.gen_name("tmp");
            //     return llvm.Core.LLVMBuildIntCast2(
            //         self.llvm_context.builder,
            //         lowered_expr,
            //         to_ty_llvm,
            //         if (to_ty.is_signed_int()) 1 else 0,
            //         @ptrCast(name),
            //     );
            // }
            // if (from_ty.is_int() and to_ty.is_float()) {
            //     if (from_ty.is_signed_int()) {
            //         return llvm.Core.LLVMBuildSIToFP(
            //             self.llvm_context.builder,
            //             lowered_expr,
            //             to_ty_llvm,
            //             @ptrCast(try self.gen_name("tmp"))
            //         );
            //     } else {
            //         return llvm.Core.LLVMBuildUIToFP(
            //             self.llvm_context.builder,
            //             lowered_expr,
            //             to_ty_llvm,
            //             @ptrCast(try self.gen_name("tmp")),
            //         );
            //     }
            // }
            // if (from_ty.is_float() and to_ty.is_int()) {
            //     if (to_ty.is_signed_int()) {
            //         return llvm.Core.LLVMBuildFPToSI(
            //             self.llvm_context.builder,
            //             lowered_expr,
            //             to_ty_llvm,
            //             @ptrCast(try self.gen_name("tmp")),
            //         );
            //     } else {
            //         return llvm.Core.LLVMBuildFPToUI(
            //             self.llvm_context.builder,
            //             lowered_expr,
            //             to_ty_llvm,
            //             @ptrCast(try self.gen_name("tmp")),
            //         );
            //     }
            // }
            // if (from_ty.is_float() and to_ty.is_float()) {
            //     if (from_ty.get_bits(self.target_size) < to_ty.get_bits(self.target_size)) {
            //         return llvm.Core.LLVMBuildFPTrunc(
            //             self.llvm_context.builder,
            //             lowered_expr,
            //             to_ty_llvm,
            //             @ptrCast(try self.gen_name("tmp")),
            //         );
            //     } else {
            //         return llvm.Core.LLVMBuildFPExt(
            //             self.llvm_context.builder,
            //             lowered_expr,
            //             to_ty_llvm,
            //             @ptrCast(try self.gen_name("tmp")),
            //         );
            //     }
            // }
        },
        .fn_call => |call| {
            const fn_value = try self.lower_local(call.left);
            const fn_type = try self.createTypeRef(self.comp_unit.type_table.get(call.left.tyid.?).?);
            const return_type = llvm.Core.LLVMGetReturnType(fn_type);
            var args = try self.allocator.alloc(llvm.Core.LLVMValueRef, call.params.len);
            defer self.allocator.free(args);
            for (call.params, 0..call.params.len) |param, i| {
                args[i] = try self.lower_local(param);
            }
            var name: []const u8 = "";
            if (node_type.base_type != .primitive or node_type.base_type.primitive != .Unit) {
                name = try self.gen_name("tmp");
            }
            const llvm_call = llvm.Core.LLVMBuildCall2(
                self.llvm_context.builder,
                fn_type,
                fn_value,
                args.ptr,
                @intCast(args.len),
                @ptrCast(name),
            );
            _ = try self.put_symbol(name, .{.value = llvm_call, .ty = return_type, .is_alloca = false});
            if (node_type.base_type == .primitive and node_type.base_type.primitive == .Unit) {
                return null;
            }
            return llvm_call;
        },
        .if_stmt => |stmt| {
            const cond = try self.lower_local(stmt.condition);
            const then_block = llvm.Core.LLVMAppendBasicBlockInContext(self.llvm_context.context, self.llvm_context.current_function, @ptrCast(try self.gen_name("then")));
            const else_block = llvm.Core.LLVMAppendBasicBlockInContext(self.llvm_context.context, self.llvm_context.current_function, @ptrCast(try self.gen_name("else")));
            const merge_block = llvm.Core.LLVMAppendBasicBlockInContext(self.llvm_context.context, self.llvm_context.current_function, @ptrCast(try self.gen_name("merge")));
            _ = llvm.Core.LLVMBuildCondBr(self.llvm_context.builder, cond, then_block, else_block);
            llvm.Core.LLVMPositionBuilderAtEnd(self.llvm_context.builder, then_block);
            const out = try self.lower_local(stmt.block);
            _ = llvm.Core.LLVMBuildBr(self.llvm_context.builder, merge_block);
            if (stmt.else_block) |else_blk|  {
                llvm.Core.LLVMPositionBuilderAtEnd(self.llvm_context.builder, else_block);
                _ = try self.lower_local(else_blk);
                _ = llvm.Core.LLVMBuildBr(self.llvm_context.builder, merge_block);
            }

            llvm.Core.LLVMPositionBuilderAtEnd(self.llvm_context.builder, merge_block);
            return out;


        },
        else => |val| {
            std.debug.print("Unhandled switch case: {s}\n", .{@tagName(val)});
            unreachable;
        },
    }
    std.debug.print("Should not reach end of function with: {any}\n", .{ast});
    unreachable;
}


