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
    current_scope: types.SymbolScope = .this,
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
                        .ast = tree,
                        .scope = .static
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
                .int_literal => out_type.base_type = .{ .primitive = .I32 }, 
                .float_literal => out_type.base_type = .{ .primitive = .F32 },
                .string_literal, .raw_string_literal => {
                    out_type.base_type = .{ .primitive = .Rune };
                    var modifiers = std.ArrayList(ast.TypeModifier).init(self.gpa);
                    try modifiers.append(.Slice);
                    out_type.modifiers = try modifiers.toOwnedSlice();
                },
                .char_literal => out_type.base_type = .{ .primitive = .Rune },
                .keyword_true, .keyword_false => out_type.base_type = .{ .primitive = .Bool },
                .ident => {
                    const val = self.get(term.span.get_string(self.source));
                    if (val == null) {
                        try self.session.emit(.Error, term.span, "Unknown Identifier (Type Check)");
                        return error.UnknownIdentifier;
                    }
                    const nval = val.?;
                    if (nval.ty == null) {
                        _ = try analyze(self, nval.ast);
                    }
                    if (self.current_scope != nval.scope) {
                        const current_scope = @intFromEnum(self.current_scope);
                        const val_scope = @intFromEnum(nval.scope);
                        if (current_scope > val_scope) {
                            nval.scope = @enumFromInt(current_scope);
                        } 
                    }
                    out_type = self.type_map.get(nval.ty.?).?;
                },
                else => {}
            }
            _ = try self.type_map.getOrPutValue(out_type.hash(), out_type);
            const out_hash = out_type.hash();
            tree.tyid = out_hash;
            return out_hash;
        },
        .terminated => |expr| {
            const tyid = try analyze(self, expr);
            const ty = self.type_map.get(tyid).?;
            if (ty.base_type == .primitive and ty.base_type.primitive == .Never) {
                tree.tyid = tyid;
                return tyid;
            }
            const out_hash = ast.Type.createPrimitive(.Unit, null).hash();
            tree.tyid = out_hash;
            return out_hash;
        },
        .binary_expr => |expr| {
            const left = try analyze(self, expr.left);
            const right = try analyze(self, expr.right);
            switch (expr.op.tag) {
                .plus, .minus, .slash, .star, .caret, .percent,
                .pipe, .amp, .shl, .shr => {
                    try check_type_equality(self, tree.span, left, right);
                    tree.tyid = left;
                    return left;
                },
                .eq2, .noteq, .lt, .lteq, .gt, .gteq => {
                    try check_type_equality(self, tree.span, left, right);
                    const out_hash = ast.Type.createPrimitive(.Bool, null).hash();
                    tree.tyid = out_hash;
                    return out_hash;
                },
                .pipe2, .amp2 => {
                    try check_type_equality(self, tree.span, ast.Type.createPrimitive(.Bool, null).hash(), left);
                    const out_hash = ast.Type.createPrimitive(.Bool, null).hash();
                    tree.tyid = out_hash;
                    return out_hash;
                },
                else => unreachable
            }
        },
        .unary_expr => |expr| {
            var ty = self.type_map.get(try analyze(self, expr.expr)).?;
            switch (expr.op.tag) {
                .minus => {
                    if (ty.base_type == .primitive and !ty.base_type.primitive.is_signed_int()) {
                        ty.base_type.primitive = ty.base_type.primitive.switch_sign();
                        const out_hash = ty.hash();
                        tree.tyid = out_hash;
                        return out_hash;
                    }
                    const out_hash = ty.hash();
                    tree.tyid = out_hash;
                    return out_hash;
                },
                .tilde => {
                    const out_hash = ty.hash();
                    tree.tyid = out_hash;
                    return out_hash;
                },
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
                    const out_hash = ty.hash();
                    tree.tyid = out_hash;
                    return out_hash;
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
                    const out_hash = ty.hash();
                    tree.tyid = out_hash;
                    return out_hash;
                },
                .bang => {
                    const out_hash = ast.Type.createPrimitive(.Bool, null).hash();
                    tree.tyid = out_hash;
                    return out_hash;
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
                        .ast = tree,
                        .scope = .parent
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
                const out_hash = ast.Type.createPrimitive(.Unit, null).hash();
                tree.tyid = out_hash;
                return out_hash;
            }
            return error.Unknown;
        },
        .block => |*expr| {
            if (expr.exprs.len == 0) {
                try self.session.emit(.Warning, tree.span, "Empty block");
                const out_hash = ast.Type.createPrimitive(.Unit, null).hash();
                tree.tyid = out_hash;
                return out_hash;

            }
            try self.symtab.append(types.SymTab.init(self.gpa));
            for(expr.exprs[0..expr.exprs.len - 1]) |exp| {
                try check_type_equality(self, tree.span, ast.Type.createPrimitive(.Unit, null).hash(), try analyze(self, exp) );
            }
            const saved_scope = self.current_scope;
            self.current_scope = .parent;
            const out = try analyze(self, expr.exprs[expr.exprs.len - 1]);
            self.current_scope = saved_scope;
            _ = self.symtab.pop();
            expr.exprs[expr.exprs.len - 1] = try mem.createWith(self.gpa, Ast.create(.{.return_stmt = expr.exprs[expr.exprs.len - 1]}, expr.exprs[expr.exprs.len - 1].span));
            expr.exprs[expr.exprs.len - 1].tyid = out;
            tree.tyid = out;
            return out;
        },
        .assignment => |stmt| {
            const lvalue = try analyze(self, stmt.lvalue);
            const assigned = try analyze(self, stmt.expr);
            try check_type_equality(self, tree.span, lvalue, assigned);
            const out_hash = ast.Type.createPrimitive(.Unit, null).hash();
            tree.tyid = out_hash;
            return out_hash;
        },
        .if_stmt => |stmt| {
            const cond = try analyze(self, stmt.condition);
            const bol = ast.Type.createPrimitive(.Bool, null).hash();
            try check_type_equality(self, tree.span, cond, bol);
            const blk = try analyze(self, stmt.block);
            if (stmt.else_block == null) {
                const out_hash = ast.Type.createPrimitive(.Unit, null).hash();
                tree.tyid = out_hash;
            }
            const else_block = stmt.else_block.?;
            try check_type_equality(self, tree.span, blk, try analyze(self, else_block));
            tree.tyid = blk;
            return blk;
        },
        .while_loop => |stmt| {
            const cond = try analyze(self, stmt.condition);
            const bol = ast.Type.createPrimitive(.Bool, null).hash();
            try check_type_equality(self, tree.span, cond, bol);
            const blk = try analyze(self, stmt.block);
            try check_type_equality(self, tree.span, blk, ast.Type.createPrimitive(.Unit, null).hash());
            const out_hash =  ast.Type.createPrimitive(.Unit, null).hash();
            tree.tyid = out_hash;
            return out_hash;
        },
        .ternary => |expr| {
            const bol = ast.Type.createPrimitive(.Bool, null).hash();
            try check_type_equality(self, tree.span, try analyze(self, expr.condition), bol);
            const out = try analyze(self, expr.true_path);
            try check_type_equality(self, tree.span,
                out,
                try analyze(self, expr.false_path)
            );
            tree.tyid = out;
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
            const out_hash = ast.Type.createPrimitive(.Unit, null).hash();
            tree.tyid = out_hash;
            return out_hash;
        },
        .param_list => |*expr| {
            const left_tyid = try analyze(self, expr.left);
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
            tree.tyid = left_tyid;
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
            const out_hash = ast.Type.createPrimitive(.Unit, null).hash();
            tree.tyid = out_hash;
            return out_hash;
        },
        .unit => {
            const out_hash = ast.Type.createPrimitive(.Unit, null).hash();
            tree.tyid = out_hash;
            return out_hash;
        },
        .return_stmt => |ret| {
            if (self.in_func) |ret_ty| {
                const saved_scope = self.current_scope;
                self.current_scope = .parent;
                const expr_ty = try analyze(self, ret);
                self.current_scope = saved_scope;
                try check_type_equality(self, tree.span, ret_ty, expr_ty);
                const out_hash = ast.Type.createPrimitive(.Never, null).hash();
                tree.tyid = out_hash;
                return out_hash;
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
            tree.tyid = cons.ty;
            return cons.ty;
        },
        .access_operator => |exp| {
            const left_tyid = try analyze(self, exp.left);
            const left_ty_prev = self.type_map.get(left_tyid).?;
            var left_ty = left_ty_prev;
            if (left_ty.base_type == .user) {
                left_ty = self.type_map.get(self.get(left_ty.base_type.user.value).?.ty.?).?;
            }
            switch (left_ty.base_type) {
                .strct => |strct| {
                    if (strct.fields.get(exp.right.value)) |field| {
                        tree.tyid = field;
                        return field;
                    }
                    try self.session.emit(.Error, exp.right.span, "Struct has no such field");
                    return error.UnknownStructField;
                },
                .@"enum" => |enm| {
                    if (enm.variants.get(exp.right.value)) |_| {
                        if (exp.left.node == .type_literal or exp.left.node == .terminal) {
                            tree.node = .{ .enum_cons = .{
                                .ty = left_ty_prev.hash(),
                                .ident = exp.right,
                                .init = null
                            }};
                        }
                        const out_hash = left_ty_prev.hash();
                        tree.tyid = out_hash;
                        return out_hash;
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
            const out_hash = out.hash();
            tree.tyid = out_hash;
            return out_hash;
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


