const std = @import("std");
const Hir = @import("Hir.zig");
const Ast = @import("Ast.zig");
const types = @import("types.zig");
const mem = @import("mem.zig");
const diag = @import("diag.zig");


symtree: std.ArrayList(types.SymbolTable),
type_map: types.TypeTbl,
hir_table: Hir.HirInfoTable,
source: []const u8,
gpa: std.mem.Allocator,
expected_type: ?Ast.TypeId = null,
function_return_type: ?Ast.TypeId = null,
session: *diag.Session,
in_loop: bool = false,
in_assignment: bool = false,
in_global_scope: bool = true,
current_scope: usize = 0,

pub fn init_context(symtree: std.ArrayList(types.SymbolTable),
        hir_table: Hir.HirInfoTable,
        type_map: types.TypeTbl,
        source: []const u8,
        gpa: std.mem.Allocator,
        session: *diag.Session,
        ) @This() {
    return .{
        .symtree = symtree,
        .type_map = type_map,
        .source = source,
        .gpa = gpa,
        .session = session,
        .hir_table = hir_table
    };
}

fn get_symbol(self: *@This(), defid: Hir.DefId) ?*types.Symbol {
    var current: ?usize = self.current_scope;
    while (current != null): (current = self.symtree.items[current.?].parent) {
        if (self.symtree.items[current.?].symbol_map.contains(defid)) {
            return self.symtree.items[current.?].symbol_map.getPtr(defid);
        }
    }
    return null;
}

fn type_check_expect(self: *@This(), tree: Hir.Hir, ty: Ast.TypeId) !Ast.TypeId {
    const previous_expected_type = self.expected_type;
    self.expected_type = ty;
    const out =try self.type_check(tree);
    self.expected_type = previous_expected_type;
    return out;
}

pub fn analyze(self: *@This(), trees: []Hir.Hir) !void {
    for (trees) |tree| {
        _ = try self.type_check(tree);
    }
}

fn type_check(self: *@This(), tree: Hir.Hir) anyerror!Ast.TypeId {
    const hir_info = self.hir_table.getPtr(tree.id).?;
    var out_type = Ast.Type.createPrimitive(.Unit, null).hash();
    switch (tree.node) {
        .top_level => |node| {
            switch (node) {
                .func => |func| {
                    if (hir_info.ty == null) {
                        var arg_types: std.ArrayList(Ast.TypeId) = .init(self.gpa);
                        for (func.parameters) |param| {
                            try arg_types.append(param.ty);
                        }
                        const ty: Ast.Type = .{
                            .base_type = .{
                                .func = .{
                                    .args = try arg_types.toOwnedSlice(),
                                    .ret = func.return_type,
                                }
                            },
                            .modifiers = null,
                        };
                        hir_info.ty = ty.hash();
                    }
                    if (func.body) |body| {
                        const saved_scope = self.current_scope;
                        self.current_scope = hir_info.scope_id;
                        const saved_function_return_type = self.function_return_type;
                        self.function_return_type = hir_info.ty;
                        _ = try self.type_check(body);
                        self.function_return_type = saved_function_return_type;
                        self.current_scope = saved_scope;
                    }
                },
                .type_decl => |_| {},
                .binding => |bind| {
                    if (hir_info.ty) |tyid| {
                        _ = try self.type_check_expect(bind.expr, tyid);
                    } else {
                        const saved_type_expect = self.expected_type;
                        self.expected_type = null;
                        hir_info.ty = try self.type_check(bind.expr);
                        self.expected_type = saved_type_expect;
                    }
                },
                .terminated => |term| {
                    _ = try self.type_check(term.*);
                },
                .return_stmt => |stmt| {
                    const val = try self.type_check_expect(stmt.expr, self.function_return_type.?);
                    if (val != self.function_return_type.?) {
                        try self.session.emit(.Error, hir_info.span, 
                            "Return expression type does not match the return type of the function");
                        return error.TypeMismatch;
                    }
                },
                else => |v| {
                    std.debug.print("Unhandled case: {any}\n", .{v});
                    unreachable;
                }
            }
        },
        .inline_expr => |node| {
            switch (node) {
                .terminal => |term| {
                    var ty: Ast.TypeId = Ast.Type.createPrimitive(.Unit, null).hash();
                    switch (term.*) {
                        .bool_literal => ty = Ast.Type.createPrimitive(.Bool, null).hash(),
                        .char_literal => |v| {
                            ty = switch (v) {
                                0...255 => Ast.Type.createPrimitive(.U8, null).hash(),
                                else => Ast.Type.createPrimitive(.I32, null).hash(),
                            };
                        },
                        .float_literal => |f| {
                            ty = Ast.Type.createPrimitive(get_float_type(f), null).hash();
                        },
                        .integer_literal => |i| {
                            ty = Ast.Type.createPrimitive(get_int_type(i, false), null).hash();
                        },
                        .identifier => |id| {
                            if (self.get_symbol(id)) |sym| {
                                ty = sym.tyid.?;
                            } else {
                                try self.session.emit(.Error, hir_info.span, "Unknown Identifier");
                                return error.UnknownIdentifier;
                            }
                        },
                        .string_literal => {
                            var mods = std.ArrayList(Ast.TypeModifier).init(self.gpa);
                            try mods.append(.Slice);
                            ty = Ast.Type.createPrimitive(.U8, try mods.toOwnedSlice()).hash();
                        },
                        .unit => {},
                        .type_literal => {
                            std.debug.print("Type Literals are not yet implemented\n", .{});
                            unreachable;
                        },
                    }
                    out_type = ty;
                },
                .unary_expr => |expr| {
                    const tyid = try self.type_check(expr.expr);
                    var ty = self.type_map.get(tyid).?;
                    switch (expr.op) {
                        .BinNot => {
                            if (ty.base_type != .primitive or ty.base_type.primitive.is_int()) {
                                try self.session.emit(.Error, hir_info.span, 
                                    "Operator `~` requires an integer");
                                return error.BinNotNotInt;
                            }
                        },
                        .DeRef => {
                            if (ty.modifiers == null or 
                                !(ty.modifiers.?[0] == .Ref or ty.modifiers.?[0] == .RefMut)) {
                                try self.session.emit(.Error, hir_info.span,
                                    "Dereference of non-reference type");
                                return error.DeRefNotRef;
                            }
                            var mods = std.ArrayList(Ast.TypeModifier).fromOwnedSlice(self.gpa, ty.modifiers.?);
                            _ = mods.orderedRemove(0);
                            ty.modifiers = try mods.toOwnedSlice();
                        },
                        .Minus => {
                            if (ty.base_type == .primitive and !ty.base_type.primitive.is_signed_int()) {
                                ty.base_type.primitive = ty.base_type.primitive.switch_sign();
                            }
                        },
                        .Not => {
                            if (ty.base_type != .primitive or ty.base_type.primitive != .Bool) {
                                try self.session.emit(.Error, hir_info.span, 
                                    "The `!` operator requires a boolean");
                                return error.ExpectedBool;
                            }
                        },
                        .Ref => {
                            var mods: std.ArrayList(Ast.TypeModifier) = if (ty.modifiers) |m| .fromOwnedSlice(self.gpa, m) else .init(self.gpa);
                            try mods.insert(0, .Ref);
                            ty.modifiers = try mods.toOwnedSlice();
                        }
                    }
                    const ty_hash = ty.hash();
                    _ = try self.type_map.getOrPutValue(ty_hash, ty);
                    out_type = ty_hash;
                },
                .binary_expr => |expr| {
                    const left = try self.type_check(expr.lhs);
                    const right = try self.type_check_expect(expr.rhs, left);
                    out_type = right;
                },
                .block => |expr| {
                    const saved_scope = self.current_scope;
                    self.current_scope = hir_info.scope_id;
                    for (expr.body) |ln| {
                        if (ln.node == .top_level and ln.node.top_level == .return_stmt) {
                            if (self.function_return_type == null) {
                                try self.session.emit(.Error, hir_info.span, 
                                    "Return statement outside of function");
                                return error.ReturnOutsideFunction;
                            }
                            _ = try self.type_check_expect(ln, self.function_return_type.?);
                            continue;
                        }
                        _ = try self.type_check(ln);
                    }
                    self.current_scope = saved_scope;
                },
                .cast => |expr| {
                    _ = self.get_cast(expr.tyid, expr.expr) catch {
                        try self.session.emit(.Error, hir_info.span, 
                            "Invalid Cast");
                        return error.InvalidCast;
                    };
                    out_type = expr.tyid;
                },
                .enum_cons => |cons| {
                    const ty = self.type_map.get(cons.ty).?;
                    const variants = ty.base_type.@"enum".variants;
                    if (variants.get(cons.field)) |val| {
                        if (val) |tyid| {
                            if (cons.value == null) {
                                try self.session.emit(.Error, hir_info.span, 
                                    "Enum variant requires value");
                                return error.EnumMissingValue;
                            }
                            out_type = try self.type_check_expect(cons.value.?, tyid);
                        } else {
                            if (cons.value != null) {
                                try self.session.emit(.Error, hir_info.span,
                                    "Enum variant does not have associated value");
                                return error.EnumExtraValue;
                            }
                        }
                    } else {
                        try self.session.emit(.Error, hir_info.span,
                            "Unknown enum variant");
                        return error.UnknownEnumVariant;
                    }
                },
                .struct_cons => |cons| {
                    const ty = self.type_map.get(cons.ty).?;
                    const fields = ty.base_type.strct.fields;
                    var iter = fields.iterator();
                    var count: usize = 0;
                    while (iter.next()) |entr| {
                        count += 1;
                        if (!cons.fields.contains(entr.key_ptr.*)) {
                            try self.session.emit(.Error, hir_info.span,
                                "Missing struct field");
                            return error.MissingStructField;
                        }
                        _ = self.type_check_expect(cons.fields.get(entr.key_ptr.*).?, entr.value_ptr.*) catch {
                            try self.session.emit(.Error, hir_info.span,
                                "Type mismatch in struct initializer");
                            return error.TypeMismatch;
                        };
                    }
                    if (cons.fields.count() > count) {
                        try self.session.emit(.Error, hir_info.span,
                            "Too many struct fields");
                        return error.TooManyStructFields;
                    }
                },
                .fn_call => |call| {
                    const fn_tyid = try self.type_check(call.expr);
                    const ty = self.type_map.get(fn_tyid).?;
                    if (ty.base_type != .func) {
                        try self.session.emit(.Error, hir_info.span,
                            "Attempt to call expression that is not a function");
                        return error.NonFunctionCall;
                    }

                    const return_ty = ty.base_type.func.ret;

                    if (call.arguments.len < ty.base_type.func.args.len) {
                        try self.session.emit(.Error, hir_info.span, 
                            "Too few arguments to function");
                        return error.TooFewArguments;
                    }
                    for (call.arguments, 0..) |exp, i| {
                        const expr_ty = try self.type_check(exp);
                        if (i >= ty.base_type.func.args.len) {
                            try self.session.emit(.Error, hir_info.span,
                                "Too many arguments passed to function");
                            return error.TooManyArguments;
                        }
                        if (expr_ty != ty.base_type.func.args[i]) {
                            try self.session.emit(.Error, hir_info.span,
                                "Type mismatch in function argument");
                            return error.TypeMismatch;
                        }
                    }
                    out_type = return_ty;
                },
                //else => |v|{
                //    std.debug.print("Unhandled case: {any}\n", .{v});
                //    unreachable;
                //}
            }
        },
    }
    hir_info.ty = out_type;
    return out_type;
}
fn get_cast(self: *@This(), expected_ty: Ast.TypeId, tree: Hir.Hir) !?Hir.Cast {
    const hir_info = self.hir_table.get(tree.id).?;
    if (hir_info.ty == expected_ty) {
        return null;
    }
    const ty1 = self.type_map.get(expected_ty).?;
    const ty2 = self.type_map.get(hir_info.ty.?).?;
    const out: Hir.Cast = .{
        .expr = tree,
        .tyid = expected_ty
    };
    if (ty1.base_type == .primitive 
        and ty2.base_type == .primitive
        and (ty1.modifiers == null 
            and ty2.modifiers == null)) {
        if (ty1.base_type.primitive.is_signed_int()
            and ty2.base_type.primitive.is_int()
            and (ty1.base_type.primitive.get_bits(64) 
                >= ty2.base_type.primitive.get_bits(64))) {
            return out;
        }
        if (ty1.base_type.primitive.is_unsigned_int()
            and ty2.base_type.primitive.is_unsigned_int()
            and (ty1.base_type.primitive.get_bits(64)
                >= ty2.base_type.primitive.get_bits(64))) {
            return out;
        }
        if (ty1.base_type.primitive.is_float()
            and (ty2.base_type.primitive.is_int()
                or ty2.base_type.primitive.is_float())
            and (ty1.base_type.primitive.get_bits(64)
                >= ty2.base_type.primitive.get_bits(64))) {
            return out;
        }
    }
    return error.TypeMismatch;
}
fn pow(base: comptime_int, power: comptime_int) comptime_int {
    var out = 1;
    for (0..power) |_| {
        out = out * base;
    }
    return out;
}
fn get_int_type(value: u128, signed: bool) Ast.PrimitiveType {
    return switch (value) {
        0...pow(2, 8) - 1 => if (signed) .I8 else .U8,
        pow(2, 8)...pow(2, 16) - 1 => if (signed) .I16 else .U16,
        pow(2, 16)...pow(2, 32) - 1 => if (signed) .I32 else .U32,
        pow(2, 32)...pow(2, 64) - 1 => if (signed) .I64 else .U64,
        pow(2, 64)...pow(2, 128) - 1 => if (signed) .I128 else .U128
    };
}

fn get_float_type(value: f64) Ast.PrimitiveType {
    const conv = @as(f32, @floatCast(value));
    return if (@as(f64, conv) == value) .F32 else .F64;
}

fn get_adjustment(self: *@This(), expected_type: Ast.TypeId, node: Hir.Hir) ?[]Hir.AdjustmentStep {
    var out: std.ArrayList(Hir.AdjustmentStep) = .init(self.gpa);
    const ty1 = self.type_map.get(expected_type).?;
    const tyid2 = self.hir_table.get(node.id).?.ty.?;
    const ty2 = self.type_map.get(tyid2).?;
    if (expected_type == tyid2) {
        return null;
    }

}
