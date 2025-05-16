const std = @import("std");
const ast = @import("Ast.zig");
const Ast = ast.Ast;
const main = @import("main.zig");
const diag = @import("diag.zig");
const types = @import("types.zig");
const mem = @import("mem.zig");

pub fn init_context(source: []const u8, session: *diag.Session, type_map: *types.TypeTbl, gpa: std.mem.Allocator) !Context {
    return .{
        .symtab = .init(gpa),
        .type_map = type_map,
        .scope = 0,
        .source = source,
        .gpa = gpa,
        .in_func = null,
        .in_assignment = false,
        .in_loop = false,
        .at_global = true,
        .session = session,
    };
}

pub const Context = struct {
    symtab: std.ArrayList(types.SymTab),
    type_map: *types.TypeTbl,
    scope: usize,
    source: []const u8,
    gpa: std.mem.Allocator,
    in_func: ?ast.TypeId,
    in_loop: bool,
    in_assignment: bool,
    at_global: bool,
    session: *diag.Session,
    fn contains(self: *@This(), key: []const u8) bool {
        for (0..self.symtab.items.len) |i| {
            if (self.symtab.items[self.symtab.items.len - 1 - i].contains(key)) {
                return true;
            }
        }
    return false;
    }
    fn push(self: *@This(), key: []const u8, value: types.Symbol) !void {
        try self.symtab.items[self.symtab.items.len-1].put(key, value);
    }
    fn get(self: *@This(), key: []const u8) ?*types.Symbol {
        if (self.symtab.items.len == 0) return null;
        for (0..self.symtab.items.len) |i| {
            if (self.symtab.items[self.symtab.items.len - 1 - i].getPtr(key)) |val| {
                return val;
            }
        }
        return null;
    }
    fn getScope(self: *@This()) ?types.SymTab {
        if (self.symtab.items.len == 0) return null;
        return self.symtab.getLast();
    }
};


pub fn resolve(self: *Context, trees: []*Ast) !void {
    self.session.unfreeze();

    try resolve_global(self, trees);
    for (trees) |tree| {
        _ = try analyze(self, tree);
    }
}
fn resolve_global(self: *Context, trees: []*Ast) !void {
    try self.symtab.append(types.SymTab.init(self.gpa));
    for (trees) |tree| {
        switch (tree.node) {
            .var_decl => |decl| {
                if (!self.contains(decl.ident.span.get_string(self.source))) {
                    try self.push(decl.ident.span.get_string(self.source), .{
                        .ty = if (decl.ty) |ty| ty else null,
                        .ident = decl.ident.span,
                        .ast = tree
                    });
                } else {
                    return error.VariableShadowsPreviousDecleration;
                }
                //TODO: figure out how to deal with recursive references
            },
            .fn_decl => |decl| {
                if (!self.contains(decl.ident.span.get_string(self.source))) {
                    var args = std.ArrayList(ast.TypeId).init(self.gpa);
                    for (decl.param_types) |ty| {
                        try args.append(ty);
                    }
                    const fnctype: ast.Type = .{
                        .base_type = .{
                            .func = .{
                                .args = try args.toOwnedSlice(),
                                .ret = decl.return_ty,
                            },
                        },
                        .modifiers = null,
                    };
                    const fnctypeid = fnctype.hash();
                    _ = try self.type_map.getOrPutValue(fnctypeid, fnctype);
                    try self.push(decl.ident.span.get_string(self.source), .{
                        .ty = fnctypeid,
                        .ident = decl.ident.span,
                        .ast = tree
                    });
                } else {
                    try self.session.emit(.Error, tree.span, "Function Shadows Previous Decleration");
                    return error.FunctionShadowsPreviousDecleration;
                }
            },
            .type_decl => |decl| {
                if (self.contains(decl.ident.span.get_string(self.source))) {
                    return error.TypeShadowsPreviousDecleration;
                }
                try self.push(decl.ident.span.get_string(self.source), .{
                    .ty = decl.ty,
                    .ident = decl.ident.span,
                    .ast = tree
                });
            },
            else => |_| {
                try self.session.emit(.Error, tree.span, "Invalid operation at the global scope");
                return error.UnallowedOperation;
            }
        }
    }
}




//TODO: Do type coercian
fn analyze(self: *Context, tree: *Ast) anyerror!ast.TypeId {
    switch (tree.node) {
        .terminal => |term| {
            var out_type: ast.Type = ast.Type.createPrimitive(.Unit, null);
            switch (term.tag) {
                .int_literal => out_type.base_type = .{ .primitive = .I32 }, //TODO: automatically widen the type to acomodate a larger literal / automatically determine sign
                .float_literal => out_type.base_type = .{ .primitive = .F32 },
                .string_literal, .raw_string_literal => {
                    out_type.base_type = .{ .primitive = .U8 };
                    var modifiers = std.ArrayList(ast.TypeModifier).init(self.gpa);
                    try modifiers.append(.Slice);
                    out_type.modifiers = try modifiers.toOwnedSlice();
                },
                .char_literal => out_type.base_type = .{ .primitive = .U8 },
                .keyword_true, .keyword_false => out_type.base_type = .{ .primitive = .Bool },
                .ident => {
                    var val = self.get(term.span.get_string(self.source));
                    if (val == null) {
                        val = self.get(term.span.get_string(self.source));
                        try self.session.emit(.Error, term.span, "Unknown Identifier (Type Check)");
                        return error.UnknownIdentifier;
                    }

                    const nval = val.?;
                    if (nval.ty == null) {
                        _ = try analyze(self, nval.ast);
                    }
                    out_type = self.type_map.get(nval.ty.?).?;
                },
                else => {}
            }
            _ = try self.type_map.getOrPutValue(out_type.hash(), out_type);
            return out_type.hash();
        },
        .terminated => |expr| {
            _ = try analyze(self, expr);
            return ast.Type.createPrimitive(.Unit, null).hash();
        },
        .binary_expr => |expr| {
            const left = try analyze(self, expr.left);
            const right = try analyze(self, expr.right);
            switch (expr.op.tag) {
                .plus, .minus, .slash, .star, .caret, .percent,
                .pipe, .amp, .shl, .shr => {
                    try check_type_equality(self, tree.span, left, right);
                    return left;
                },
                .eq2, .noteq, .lt, .lteq, .gt, .gteq => {
                    try check_type_equality(self, tree.span, left, right);
                    return ast.Type.createPrimitive(.Bool, null).hash();
                },
                .pipe2, .amp2 => {
                    try check_type_equality(self, tree.span, ast.Type.createPrimitive(.Bool, null).hash(), left);
                    return ast.Type.createPrimitive(.Bool, null).hash();
                },
                else => unreachable
            }
        },
        .unary_expr => |expr| {
            var ty = self.type_map.get(try analyze(self, expr.expr)).?;
            switch (expr.op.tag) {
                .minus,
                .tilde => return ty.hash(),
                .amp => {
                    var mods: std.ArrayList(ast.TypeModifier) = undefined;
                    if (ty.modifiers) |modd| {
                        mods = .fromOwnedSlice(self.gpa, modd);
                    } else {
                        mods = .init(self.gpa);
                    }
                    try mods.insert(0,.Ref);
                    ty.modifiers = try mods.toOwnedSlice();
                    _ = try self.type_map.getOrPutValue(ty.hash() ,ty);
                    return ty.hash();
                },
                .star => {
                    if (ty.modifiers == null 
                        or ty.modifiers.?.len == 0
                        or ty.modifiers.?[0] != .Ref) {
                        try self.session.emit(.Error, expr.expr.span, "Dereference of non-reference type");
                        return error.InvalidDeref;
                    } 

                    var mods: std.ArrayList(ast.TypeModifier) = undefined;
                    if (ty.modifiers) |modd| {
                        mods = .fromOwnedSlice(self.gpa, modd);
                    } 
                    _ = mods.orderedRemove(0);
                    ty.modifiers = try mods.toOwnedSlice();
                    return ty.hash();
                },
                .bang => {
                    return ast.Type.createPrimitive(.Bool, null).hash();
                },
                else => unreachable
            }
        },
        .fn_decl => |decl| {
            if (!self.at_global) {
                if (!self.contains(decl.ident.span.get_string(self.source))) {
                    var args = std.ArrayList(ast.TypeId).init(self.gpa);
                    for (decl.param_types) |ty| {
                        try args.append(ty);
                    }
                    const func_type: ast.Type = .{
                        .base_type = .{
                            .func = .{
                                .args = try args.toOwnedSlice(),
                                .ret = decl.return_ty,
                            },
                        },
                        .modifiers = null,
                    };
                    _ = try self.type_map.getOrPutValue(func_type.hash(), func_type);
                    try self.push(decl.ident.span.get_string(self.source), .{
                        .ty = func_type.hash(),
                        .ident = decl.ident.span,
                        .ast = tree
                    });
                } 
            }

            if (decl.body) |body| {
                try self.symtab.append(types.SymTab.init(self.gpa));
                for (decl.params, 0..) |param, i| {
                    try self.push(param.span.get_string(self.source), .{
                        .ty = decl.param_types[i],
                        .ident = param.span,
                        .ast = tree
                    });
           }
                const prev_in_func = self.in_func;
                self.in_func = decl.return_ty;
                const prev_in_global = self.at_global;
                self.at_global = false;
                const ret_ty = try analyze(self, body);
                try check_type_equality(self, tree.span, decl.return_ty, ret_ty);
                self.in_func = prev_in_func;
                self.at_global = prev_in_global;
                _ = self.symtab.pop();
                return ast.Type.createPrimitive(.Unit, null).hash();
            }
            return error.Unknown;
        },
        .block => |expr| {
            if (expr.exprs.len == 0) {
                try self.session.emit(.Warning, tree.span, "Empty block");
                return ast.Type.createPrimitive(.Unit, null).hash();
            }
            try self.symtab.append(types.SymTab.init(self.gpa));
            for(expr.exprs[0..expr.exprs.len - 1]) |exp| {
                try check_type_equality(self, tree.span, ast.Type.createPrimitive(.Unit, null).hash(), try analyze(self, exp) );
            }
            const out = try analyze(self, expr.exprs[expr.exprs.len - 1]);
            _ = self.symtab.pop();
            return out;
        },
        .assignment => |stmt| {
            const lvalue = try analyze(self, stmt.lvalue);
            const assigned = try analyze(self, stmt.expr);
            try check_type_equality(self, tree.span, lvalue, assigned);
            return ast.Type.createPrimitive(.Unit, null).hash();
        },
        .if_stmt => |stmt| {
            const cond = try analyze(self, stmt.condition);
            const bol = ast.Type.createPrimitive(.Bool, null).hash();
            try check_type_equality(self, tree.span, cond, bol);
            const blk = try analyze(self, stmt.block);
            if (stmt.else_block == null) return ast.Type.createPrimitive(.Unit, null).hash();
            const else_block = stmt.else_block.?;
            try check_type_equality(self, tree.span, blk, try analyze(self, else_block));
            return blk;
        },
        .while_loop => |stmt| {
            const cond = try analyze(self, stmt.condition);
            const bol = ast.Type.createPrimitive(.Bool, null).hash();
            try check_type_equality(self, tree.span, cond, bol);
            const blk = try analyze(self, stmt.block);
            try check_type_equality(self, tree.span, blk, ast.Type.createPrimitive(.Unit, null).hash());
            return ast.Type.createPrimitive(.Unit, null).hash();
        },
        .ternary => |expr| {
            const bol = ast.Type.createPrimitive(.Bool, null).hash();
            try check_type_equality(self, tree.span, try analyze(self, expr.condition), bol);
            const out = try analyze(self, expr.true_path);
            try check_type_equality(self, tree.span,
                out,
                try analyze(self, expr.false_path)
            );
            return out;
        },
        .var_decl => |*stmt| {
            if (stmt.ty == null and stmt.initialize == null) {
                try self.session.emit(.Error, tree.span, "Variable decleration without initialization must have an explicit type");
                return error.VariableDeclerationWithoutExplicitType;
            }
            var ty = stmt.ty;
            if (ty == null) {
                ty = try analyze(self, stmt.initialize.?);
                stmt.ty = ty;
            } else {
                const initty = try analyze(self, stmt.initialize.?);
                try check_type_equality(self, tree.span, ty.?, initty);
            }
            if (!self.contains(stmt.ident.span.get_string(self.source))) {
                try self.push(stmt.ident.span.get_string(self.source), .{
                    .ty = ty,
                    .ident = stmt.ident.span,
                    .ast = tree
                });
            } else {
                var val = self.get(stmt.ident.span.get_string(self.source)).?;
                val.ty = ty;
            }
            return ast.Type.createPrimitive(.Unit, null).hash();
        },
        .param_list => |*expr| {
            const left_tyid = try analyze(self, expr.left);
            std.debug.print("DEBUG: {any}\n", .{expr.left.node});
            if (expr.left.node == .enum_cons) {
                if (expr.params.len != 1) {
                    try self.session.emit(.Error, tree.span, "Required an initialization value");
                    return error.RequiredInitialization;
                }
                if (expr.left.node.enum_cons.init == null) {
                    expr.left.node.enum_cons.init = expr.params[0];
                }
                tree.node = expr.left.node;
            }
            return left_tyid;

        },
        .type_decl => |decl| {
            if (!self.contains(decl.ident.span.get_string(self.source))) {
                try self.push(decl.ident.span.get_string(self.source), .{
                    .ty = decl.ty,
                    .ident = decl.ident.span,
                    .ast = tree,
                });
            }
            return ast.Type.createPrimitive(.Unit, null).hash();
        },
        .unit => {
            return ast.Type.createPrimitive(.Unit, null).hash();
        },
        .return_stmt => |ret| {
            if (self.in_func) |ret_ty| {
                const expr_ty = try analyze(self, ret);
                try check_type_equality(self, tree.span, ret_ty, expr_ty);
                return ast.Type.createPrimitive(.Never, null).hash();
            }
            try self.session.emit(.Error, tree.span, "return outside of function");
            return error.ReturnOutsideFunction;
        },
        .struct_cons => |cons| {
            const type_name = self.type_map.get(cons.ty).?.base_type.user.value;
            const struct_ty = self.type_map.get(self.get(type_name).?.ty.?).?.base_type.strct.fields;
            var iter = cons.fields.iterator();
            while (iter.next()) |entry| {
                const expected_ty = struct_ty.get(entry.key_ptr.*);
                if (expected_ty == null) {
                    try self.session.emit(.Error, tree.span, "Unknown struct field");
                    return error.UnknownStructField;
                }
                const actual_ty = try analyze(self, entry.value_ptr.*);
                try check_type_equality(self, tree.span, expected_ty.?, actual_ty);
            }
            return cons.ty;
        },
        .access_operator => |exp| {
            const left_tyid = try analyze(self, exp.left);
            var left_ty = self.type_map.get(left_tyid).?;
            if (left_ty.base_type == .user) {
                left_ty = self.type_map.get(self.get(left_ty.base_type.user.value).?.ty.?).?;
            }
            switch (left_ty.base_type) {
                .strct => |strct| {
                    if (strct.fields.get(exp.right.value)) |field| {
                        return field;
                    }
                    try self.session.emit(.Error, exp.right.span, "Struct has no such field");
                    return error.UnknownStructField;
                },
                .@"enum" => |enm| {
                    if (enm.variants.get(exp.right.value)) |_| {
                    std.debug.print("DEBUG ENUM: {any}\n", .{exp.left.node});
                        if (exp.left.node == .type_literal) {
                            tree.node = .{ .enum_cons = .{
                                .ty = left_ty.hash(),
                                .ident = exp.right,
                                .init = null
                            }};
                        }
                        return left_tyid;
                    }
                    try self.session.emit(.Error, exp.right.span, "Enum has no variant");
                    return error.UnknownEnumVariant;
                },
                else => {}
            }
            try self.session.emit(.Error, exp.left.span, "Type is not subscriptable");
            return error.InvalidAccess;
        },
        .type_literal => |ty| {
            const hash = ty.hash();
            _ = try self.type_map.getOrPutValue(hash, ty);
            const out: ast.Type = .{
                .base_type = .{
                    .@"type" = hash,
                },
                .modifiers = null,
                .chash = hash
            };
            return out.hash();
        },
        else => |v| {
            std.debug.print("Unhandled semantics: {any}\n", .{v});
            unreachable;
        }
    }
}

//FIXME: span is wrong
fn coerce(self: *Context, span: types.Span, left: ast.TypeId, right: ast.TypeId) !bool {
    var lft = left;
    var rit = right;
    if (self.type_map.get(left)) |leftty| {
        if (leftty.base_type == .primitive and leftty.base_type.primitive == .Never) {
            return true;
        }
        if (leftty.base_type == .user and self.contains(leftty.base_type.user.value)) {
            lft = self.get(leftty.base_type.user.value).?.ty.?;
        } else if (leftty.base_type == .user) {
            try self.session.emit(.Error, span, "User type does not exist");
            return error.UndefinedType;
        } 
    }
    if (self.type_map.get(right)) |rightty| {
        if (rightty.base_type == .primitive and rightty.base_type.primitive == .Never) {
            return true;
        }
        if (rightty.base_type == .user and self.contains(rightty.base_type.user.value)) {
            rit = self.get(rightty.base_type.user.value).?.ty.?;
        } else if (rightty.base_type == .user) {
            try self.session.emit(.Error, span, "User type does not exist");
            return error.UndefinedType;
        }
    }
    return lft == rit;
}


fn check_type_equality(self: *Context, span: types.Span, left: ast.TypeId, right: ast.TypeId) !void {
    if (!try coerce(self, span, left, right)) {
        const leftty = try self.type_map.get(left).?.get_string(self.type_map, self.gpa, self.source);

        const rightty = try self.type_map.get(right).?.get_string(self.type_map, self.gpa, self.source);
        const msg = try std.fmt.allocPrint(self.gpa,
            "Expected Type: {s}, got: {s}", 
            .{leftty, rightty}
        );
        try self.session.emit(.Error, span, msg);
        return error.TypeMismatch;
    }
}


