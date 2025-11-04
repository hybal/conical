const std = @import("std");
const Hir = @import("Hir.zig");
const Ast = @import("Ast.zig");
const types = @import("types.zig");
const mem = @import("mem.zig");
const diag = @import("diag.zig");


context: *types.Context,
hir_table: Hir.HirInfoTable,
gpa: std.mem.Allocator,


expected_type: ?Ast.TypeId = null,
function_return_type: ?Ast.TypeId = null,
function_id_span: ?types.Span = null,
in_loop: bool = false,
in_assignment: bool = false,
in_global_scope: bool = true,
current_scope: usize = 0,

pub fn init(context: *types.Context,
        hir_table: Hir.HirInfoTable,
        gpa: std.mem.Allocator,
        ) @This() {
    return .{
        .gpa = gpa,
        .hir_table = hir_table,
        .context = context,
    };
}

fn get_symbol(self: *@This(), defid: types.DefId) ?*types.Symbol {
    var current_scope = self.context.sym_tab.items[self.current_scope];
    var exit = false;
    while (!exit) {
        if (current_scope.symbol_map.getPtr(defid)) |outsym| {
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
                        self.function_return_type = func.return_type;
                        const saved_function_id_span = self.function_id_span;
                        self.function_id_span = self.get_symbol(func.id).?.name.span;
                        _ = try self.type_check(body);
                        self.function_return_type = saved_function_return_type;
                        self.current_scope = saved_scope;
                        self.function_id_span = saved_function_id_span;
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
                        self.get_symbol(bind.id).?.tyid = hir_info.ty;
                        self.expected_type = saved_type_expect;
                    }
                },
                .terminated => |term| {
                    const tyid = try self.type_check(term.*);
                    const ty = self.context.type_tab.get(tyid).?;
                    if (ty.is_never()) {
                        out_type = tyid;
                    }
                },
                //may in the future be converted into an inline expr
                .return_stmt => |stmt| {
                    const val = try self.type_check_expect(stmt.expr, self.function_return_type.?);
                    _ = try self.type_equal(self.function_return_type.?, val, self.hir_table.get(stmt.expr.id).?.span);
                    out_type = Ast.Type.createPrimitive(.Never, null).hash();
                },
                .branch => |branch| {
                    _ = try self.type_check_expect(branch.condition, Ast.Type.createPrimitive(.Bool, null).hash());
                    const a_branch = try self.type_check(branch.a_path);
                    if (branch.b_path) |b_path| {
                        _ = try self.type_check_expect(b_path, a_branch);
                    }
                    out_type = a_branch;

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
                    var ty: Ast.Type = Ast.Type.createPrimitive(.Unit, null);
                    var tyid: ?Ast.TypeId = null;
                    switch (term.*) {
                        .bool_literal => ty = Ast.Type.createPrimitive(.Bool, null),
                        .char_literal => |v| {
                            ty = switch (v) {
                                0...255 => Ast.Type.createPrimitive(.U8, null),
                                else => Ast.Type.createPrimitive(.I32, null),
                            };
                        },
                        .float_literal => |f| {
                            ty = Ast.Type.createPrimitive(get_float_type(f), null);
                        },
                        .integer_literal => |i| {
                            const int_type = get_int_type(i, false);
                            if (self.expected_type) |expected_typeid| {
                                const expected_type = self.context.type_tab.get(expected_typeid).?;
                                if (expected_type.base_type == .primitive and
                                    expected_type.base_type.primitive != int_type) {
                                    if (expected_type.is_int()) {
                                        tyid = expected_typeid;
                                    }
                                } else {
                                    try self.context.session.emit(.Error, hir_info.span, "Type mismatch");
                                    return error.TypeMismatch;
                                }
                            } else {
                                ty = Ast.Type.createPrimitive(get_int_type(i, false), null);
                            }
                        },
                        .path => |id| {
                            if (self.get_symbol(id)) |sym| {
                                tyid = sym.tyid.?;
                            } else {
                                try self.context.session.emit(.Error, hir_info.span, "Unknown Identifier");
                                return error.UnknownIdentifier;
                            }
                        },
                        .string_literal => {
                            var mods = std.ArrayList(Ast.TypeModifier).init(self.gpa);
                            try mods.append(.Ref);
                            ty = Ast.Type.createPrimitive(.U8, try mods.toOwnedSlice());
                        },
                        .unit => {},
                        .type_literal => {
                            std.debug.print("Type Literals are not yet implemented\n", .{});
                            unreachable;
                        },
                    }
                    if (tyid == null) {
                        tyid = ty.hash();
                        _ = try self.context.type_tab.getOrPutValue(tyid.?, ty);
                    }
                    if (self.expected_type) |exty| {
                        _ = try self.type_equal(exty, tyid.?, hir_info.span);
                    }
                    out_type = tyid.?;
                },
                .unary_expr => |expr| {
                    const tyid = try self.type_check(expr.expr);
                    var ty = self.context.type_tab.get(tyid).?;
                    switch (expr.op) {
                        .BinNot => {
                            if (ty.base_type != .primitive or ty.base_type.primitive.is_int()) {
                                try self.context.session.emit(.Error, hir_info.span, 
                                    "Operator `~` requires an integer");
                                return error.BinNotNotInt;
                            }
                        },
                        .DeRef => {
                            if (ty.modifiers == null or 
                                !(ty.modifiers.?[0] == .Ref or ty.modifiers.?[0] == .RefMut)) {
                                try self.context.session.emit(.Error, hir_info.span,
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
                                try self.context.session.emit(.Error, hir_info.span, 
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
                    //_ = try self.context.type_tab.getOrPutValue(ty_hash, ty);
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
                    for (expr.body, 0..) |ln, i| {
                        if (i < expr.body.len - 1) {
                            const ty = try self.type_check(ln);
                            _ = try self.type_equal(Ast.Type.createPrimitive(.Unit, null).hash(), ty, self.hir_table.get(ln.id).?.span);
                        } else {
                            const ty = try self.type_check(ln);
                            if (self.function_return_type) |ret| {
                                _ = try self.type_equal(ret, ty, self.function_id_span.?);
                            }
                            out_type = ty;

                                
                        }
                    }
                    self.current_scope = saved_scope;
                },
                .cast => |expr| {
                    _ = self.get_cast(expr.tyid, expr.expr) catch {
                        try self.context.session.emit(.Error, hir_info.span, 
                            "Invalid Cast");
                        return error.InvalidCast;
                    };
                    out_type = expr.tyid;
                },
                .enum_cons => |cons| {
                    const ty = self.context.type_tab.get(cons.ty).?;
                    const variants = ty.base_type.@"enum".variants;
                    if (variants.get(cons.field)) |val| {
                        if (val) |tyid| {
                            if (cons.value == null) {
                                try self.context.session.emit(.Error, hir_info.span, 
                                    "Enum variant requires value");
                                return error.EnumMissingValue;
                            }
                            out_type = try self.type_check_expect(cons.value.?, tyid);
                        } else {
                            if (cons.value != null) {
                                try self.context.session.emit(.Error, hir_info.span,
                                    "Enum variant does not have associated value");
                                return error.EnumExtraValue;
                            }
                        }
                    } else {
                        try self.context.session.emit(.Error, hir_info.span,
                            "Unknown enum variant");
                        return error.UnknownEnumVariant;
                    }
                },
                .struct_cons => |cons| {
                    const ty = self.context.type_tab.get(cons.ty).?;
                    const fields = ty.base_type.strct.fields;
                    var iter = fields.iterator();
                    var count: usize = 0;
                    while (iter.next()) |entr| {
                        count += 1;
                        if (!cons.fields.contains(entr.key_ptr.*)) {
                            try self.context.session.emit(.Error, hir_info.span,
                                "Missing struct field");
                            return error.MissingStructField;
                        }
                        _ = try self.type_check_expect(cons.fields.get(entr.key_ptr.*).?, entr.value_ptr.*);
                    }
                    if (cons.fields.count() > count) {
                        try self.context.session.emit(.Error, hir_info.span,
                            "Too many struct fields");
                        return error.TooManyStructFields;
                    }
                },
                .fn_call => |call| {
                    const fn_tyid = try self.type_check(call.expr);
                    const ty = self.context.type_tab.get(fn_tyid).?;
                    if (ty.base_type != .func) {
                        try self.context.session.emit(.Error, hir_info.span,
                            "Attempt to call expression that is not a function");
                        return error.NonFunctionCall;
                    }

                    const return_ty = ty.base_type.func.ret;

                    if (call.arguments.len < ty.base_type.func.args.len) {
                        try self.context.session.emit(.Error, hir_info.span, 
                            "Too few arguments to function");
                        return error.TooFewArguments;
                    }
                    for (call.arguments, 0..) |exp, i| {
                        const expr_ty = try self.type_check_expect(exp, ty.base_type.func.args[i]);
                        if (i >= ty.base_type.func.args.len) {
                            try self.context.session.emit(.Error, hir_info.span,
                                "Too many arguments passed to function");
                            return error.TooManyArguments;
                        }
                        _ = try self.type_equal(ty.base_type.func.args[i], expr_ty, self.hir_table.get(exp.id).?.span);

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
    const ty1 = self.context.type_tab.get(expected_ty).?;
    const ty2 = self.context.type_tab.get(hir_info.ty.?).?;
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

fn type_equal(self: *@This(), expected_type: Ast.TypeId, actual_type: Ast.TypeId, span: types.Span) !bool {
    if (expected_type == actual_type) {
        return true;
    }
    const expected = self.context.type_tab.get(expected_type).?;
    const actual = self.context.type_tab.get(actual_type).?;
    if (expected.is_never() or actual.is_never()) {
        return true;
    }
    const adjustment = try self.get_adjustment(expected_type, actual_type);
    if (adjustment == null) {
        try self.context.session.emit(.Error, span, 
            try std.fmt.allocPrint(self.gpa, "Type Mismatch, expected type: `{s}`, got: `{s}`",
                .{try self.context.type_tab.get(expected_type).?
                    .get_string(&self.context.type_tab, self.gpa, self.context.source),
                    try self.context.type_tab.get(actual_type).?
                        .get_string(&self.context.type_tab, self.gpa, self.context.source)}));
        return error.TypeMismatch;
    }
    return false;

}

fn get_adjustment(self: *@This(), expected_type: Ast.TypeId, actual_type: Ast.TypeId) !?[]Hir.AdjustmentStep {
    const expected = self.context.type_tab.get(expected_type).?;
    const actual = self.context.type_tab.get(actual_type).?;
    var adjustments: std.ArrayList(Hir.AdjustmentStep) = .init(self.gpa);
    if (actual.modifiers != null and actual.modifiers.?.len > 0) {
        const actual_mods = actual.modifiers.?;
        if (expected.modifiers != null and expected.modifiers.?.len > 0) {
            const expected_mods = expected.modifiers.?;
            var i: usize = actual_mods.len - 1;
            var j: usize = expected_mods.len - 1;
            while (i >= 0 and j > 0) {
                const actual_mod = actual_mods[i];
                const expected_mod = expected_mods[j];
                if (actual_mod.equals(expected_mod)) {
                    i -= 1;
                    j -= 1;
                } else if (actual_mod == .RefMut and expected_mod == .Ref) {
                    i -= 1;
                    j -= 1;
                    try adjustments.append(.RefMutDiscard);
                } else if (actual_mod == .Mut and expected_mod != .Mut) {
                    i -= 1;
                    try adjustments.append(.MutDiscard);
                } else if (actual_mod == .Ref and expected_mod != .Ref) {
                    i -= 1;
                    try adjustments.append(.AutoDeref);
                } else if (expected_mod == .Ref and actual_mod != .Ref) {
                    j -= 1;
                    try adjustments.append(.AutoRef);
                } else {
                    return null;
                }
            }
        } else {
            var i: usize = actual_mods.len - 1;
            while (i >= 0) {
                const actual_mod = actual_mods[i];
                if (actual_mod == .Ref or actual_mod == .RefMut) {
                    if (actual_mod == .RefMut) {
                        try adjustments.append(.RefMutDiscard);
                    }
                    try adjustments.append(.AutoDeref);
                } else if (actual_mod == .Mut) {
                    try adjustments.append(.MutDiscard);
                } else {
                    return null;
                }
                i -= 1;
            }
        }
    } else {
        if (expected.modifiers != null and expected.modifiers.?.len > 0) {
            const expected_mods = expected.modifiers.?;
            var i: usize = expected_mods.len - 1;
            while (i >= 0) {
                const expected_mod = expected_mods[i];
                if (expected_mod == .Ref) {
                    try adjustments.append(.AutoDeref);
                } else {
                    return null;
                }
                i -= 1;
            }
        }
    } 
    const expected_base = expected.base_type;
    const actual_base = actual.base_type;
    if (expected_base.equals(&actual_base)) {
        return try adjustments.toOwnedSlice();
    }
    if (expected.is_signed_int() 
        and actual.is_int()
        and expected.base_type.primitive.get_bits(64)
        >= actual.base_type.primitive.get_bits(64)) {
        try adjustments.append(.{ .NumericCast = expected_type });
    } else if (expected.is_unsigned_int()
        and actual.is_unsigned_int()
        and expected.base_type.primitive.get_bits(64)
        >= actual.base_type.primitive.get_bits(64)) {
        try adjustments.append(.{ .NumericCast = expected_type });
    } else if (expected.is_float()
        and (actual.is_int() or actual.is_float())
        and expected.base_type.primitive.get_bits(64)
        >= actual.base_type.primitive.get_bits(64)) {
        try adjustments.append(.{.NumericCast = expected_type});
    } else {
        return null;
    }
    return try adjustments.toOwnedSlice();
}
