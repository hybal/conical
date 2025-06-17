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
        .in_func = false,
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
    in_func: bool,
    expected_type: ?ast.TypeId = null,
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
                    tree.tyid = fnctypeid;
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



fn analyze_expect(self: *Context, tree: *Ast, expected_type: ast.TypeId) !ast.TypeId {
    const saved_expected = self.expected_type;
    self.expected_type = expected_type;
    const out =try analyze(self, tree);
    self.expected_type = saved_expected;
    return out;
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
                        nval.ty = try analyze(self, nval.ast);
                    }

                    if (self.current_scope != nval.scope) {
                        const current_scope = @intFromEnum(self.current_scope);
                        const val_scope = @intFromEnum(nval.scope);
                        if (current_scope > val_scope) {
                            nval.scope = @enumFromInt(current_scope);
                        } 
                    }
                    out_type = self.type_map.get(nval.ty.?).?;
                    if (self.in_assignment and (out_type.modifiers == null or out_type.modifiers.?[0] != .Mut)) {
                        try self.session.emit(.Error, term.span, "Assignment to immutable variable.");
                        return error.ImmutableAssignment;
                    }
                },
                else => unreachable,
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
            switch (expr.op.tag) {
                .plus, .minus, .slash, .star, .caret, .percent,
                .pipe, .amp, .shl, .shr => {
                    _ = try analyze_expect(self, expr.right, left);
                    tree.tyid = left;
                    return left;
                },
                .eq2, .noteq, .lt, .lteq, .gt, .gteq => {
                    _ = try analyze_expect(self, expr.right, left);
                    const out_hash = ast.Type.createPrimitive(.Bool, null).hash();
                    tree.tyid = out_hash;
                    return out_hash;
                },
                .pipe2, .amp2 => {
                    const out_hash = ast.Type.createPrimitive(.Bool, null).hash();
                    try check_type_equality(self, tree, tree.span, out_hash, left);
                    _ = try analyze_expect(self, expr.right, out_hash);
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
                    _ = try analyze_expect(self, expr.expr, out_hash);
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
                self.in_func = true;
                const prev_in_global = self.at_global;
                self.at_global = false;
                _ = try analyze_expect(self, body, decl.return_ty);
                self.in_func = prev_in_func;
                self.at_global = prev_in_global;
                _ = self.symtab.pop();
                const out_hash = ast.Type.createPrimitive(.Unit, null).hash();
                tree.tyid = out_hash;
                return out_hash;
            } else if (decl.decl_mod != null and decl.decl_mod == .Extern) {
                const out_hash = ast.Type.createPrimitive(.Unit, null).hash();
                tree.tyid = out_hash;
                return out_hash;
            }
            return error.Unsupported;
        },
        .block => |*expr| {
            if (expr.exprs.len == 0) {
                try self.session.emit(.Warning, tree.span, "Empty block");
                const out_hash = ast.Type.createPrimitive(.Unit, null).hash();
                tree.tyid = out_hash;
                return out_hash;

            }
            try self.symtab.append(types.SymTab.init(self.gpa));
            if (expr.exprs.len > 1) {
                for(expr.exprs[0..expr.exprs.len - 1]) |exp| {
                    try check_type_equality(self, tree, tree.span, ast.Type.createPrimitive(.Unit, null).hash(), try analyze(self, exp) );
                }
            }
            const saved_scope = self.current_scope;
            self.current_scope = .parent;
            const out = try analyze(self, expr.exprs[expr.exprs.len - 1]);
            if (self.in_func and self.expected_type != null) {
                try check_type_equality(self, tree, tree.span, self.expected_type.?, out); 
            }
            self.current_scope = saved_scope;
            _ = self.symtab.pop();
            var last_expr_copy = Ast.create(tree.node.block.exprs[expr.exprs.len - 1].node, expr.exprs[expr.exprs.len - 1].span);
            last_expr_copy.tyid = expr.exprs[expr.exprs.len - 1].tyid;
            const return_stmt = Ast.create(.{.return_stmt = try mem.createWith(self.gpa, last_expr_copy)}, last_expr_copy.span);
            const return_node = try mem.createWith(self.gpa, return_stmt);
            return_node.tyid = ast.Type.createPrimitive(.Never, null).hash();
            expr.exprs[expr.exprs.len - 1] = return_node;
            tree.tyid = out;

            return out;
        },
        .assignment => |stmt| {
            const saved_assignment = self.in_assignment;
            self.in_assignment = true;
            const lvalue = try analyze(self, stmt.lvalue);
            self.in_assignment = saved_assignment;
            _ = try analyze_expect(self, stmt.expr, lvalue);
            const out_hash = ast.Type.createPrimitive(.Unit, null).hash();
            tree.tyid = out_hash;
            return out_hash;
        },
        .if_stmt => |stmt| {
            const cond = try analyze(self, stmt.condition);
            const bol = ast.Type.createPrimitive(.Bool, null).hash();
            try check_type_equality(self, tree, tree.span, cond, bol);
            const blk = try analyze(self, stmt.block);
            if (stmt.else_block == null) {
                const out_hash = ast.Type.createPrimitive(.Unit, null).hash();
                tree.tyid = out_hash;
            }
            const else_block = stmt.else_block.?;
            try check_type_equality(self, tree, tree.span, blk, try analyze(self, else_block));
            tree.tyid = blk;
            return blk;
        },
        .while_loop => |stmt| {
            const cond = try analyze(self, stmt.condition);
            const bol = ast.Type.createPrimitive(.Bool, null).hash();
            try check_type_equality(self, tree, tree.span, cond, bol);
            const blk = try analyze(self, stmt.block);
            try check_type_equality(self, tree, tree.span, blk, ast.Type.createPrimitive(.Unit, null).hash());
            const out_hash =  ast.Type.createPrimitive(.Unit, null).hash();
            tree.tyid = out_hash;
            return out_hash;
        },
        .ternary => |expr| {
            const bol = ast.Type.createPrimitive(.Bool, null).hash();
            try check_type_equality(self, tree, tree.span, try analyze(self, expr.condition), bol);
            const out = try analyze(self, expr.true_path);
            try check_type_equality(self, tree, tree.span,
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
            } else if (stmt.initialize != null) {
                _ = try analyze_expect(self, stmt.initialize.?, ty.?);
            }
            if (stmt.is_mut) {
                var typ = self.type_map.get(ty.?).?;
                var mods: ?std.ArrayList(ast.TypeModifier) = null;
                if (typ.modifiers) |typmods| {
                    mods = .fromOwnedSlice(self.gpa, typmods);
                } else {
                    mods = .init(self.gpa);
                }
                try mods.?.insert(0,.Mut);
                typ.modifiers = try mods.?.toOwnedSlice();
                const hash = typ.hash();
                _ = try self.type_map.getOrPutValue(hash, typ);
                ty = hash;
                stmt.ty = ty;
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
        .param_list => |expr| {
            const left_tyid = try analyze(self, expr.left);
            const left_ty = self.type_map.get(left_tyid).?;
            if (expr.left.node == .enum_cons) {
                if (expr.params.len != 1) {
                    try self.session.emit(.Error, tree.span, "Required an initialization value");
                    return error.RequiredInitialization;
                }
                if (expr.left.node.enum_cons.init == null) {
                    expr.left.node.enum_cons.init = expr.params[0];
                }
                tree.node = expr.left.node;
                tree.tyid = left_tyid;
                return left_tyid;
            } else if (left_ty.base_type == .func) {
                if (left_ty.base_type != .func) {
                    try self.session.emit(.Error, tree.span, "Only functions can be called");
                    return error.NonFunctionCall;
                }
                const func = left_ty.base_type.func;
                if (func.args.len != expr.params.len) {
                    try self.session.emit(.Error, tree.span, "Function call with incorrect number of arguments");
                    return error.IncorrectArguments;
                }
                for (expr.params, 0..) |param, i| {
                    _ = try analyze_expect(self, param, func.args[i]);
                }
                tree.node = .{ .fn_call = expr };
                tree.tyid = func.ret;
                return func.ret;
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
        .cast => |expr| {
            const ty = self.type_map.get(expr.ty).?;
            const expr_ty = self.type_map.get(try analyze(self, expr.expr)).?;
            if (ty.base_type != .primitive or expr_ty.base_type != .primitive) {
                try self.session.emit(.Error, tree.span, "Only primitive types can be cast");
                return error.NonPrimitiveCast;
            }
            tree.tyid = expr.ty;
            return expr.ty;
        },
        .return_stmt => |ret| {
            if (self.in_func) {
                const saved_scope = self.current_scope;
                self.current_scope = .parent;
                _ =  try analyze_expect(self, ret, self.expected_type.?);
                self.current_scope = saved_scope;
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
                try check_type_equality(self, tree, tree.span, expected_ty.?, actual_ty);
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
        .fn_call => |call| {
            const fn_tyid = try analyze(self, call.left);
            const fn_ty = self.type_map.get(fn_tyid).?;
            if (fn_ty.base_type != .func) {
                try self.session.emit(.Error, tree.span, "Only functions can be called");
                return error.NonFunctionCall;
            }
            const func = fn_ty.base_type.func;
            if (func.args.len != call.params.len) {
                try self.session.emit(.Error, tree.span, "Function call with incorrect number of arguments");
                return error.IncorrectArguments;
            }
            for (call.params, 0..) |param, i| {
                _ = try analyze_expect(self, param, func.args[i]);
            }
            tree.tyid = func.ret;
            return func.ret;
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


const coercion_matrix: [16][16]bool = .{
    // To:      I8   I16  I32  I64  I128 Isize U8   U16  U32  U64  U128 Usize F32  F64  Bool Rune
    // I8    
    .{ true, true, true, true, true, true, false,false,false,false,false,false,true, true, false,true },
    // I16   
    .{ false,true, true, true, true, true, false,false,false,false,false,false,true, true, false,true },
    // I32   
    .{ false,false,true, true, true, true, false,false,false,false,false,false,true, true, false,true },
    // I64   
    .{ false,false,false,true, true, true, false,false,false,false,false,false,false,true, false,false },
    // I128  
    .{ false,false,false,false,true, false,false,false,false,false,false,false,false,false,false,false },
    // Isize 
    .{ false,false,false,false,false,true, false,false,false,false,false,false,false,false,false,false },
    // U8    
    .{ true, true, true, true, true, true, true, true, true, true, true, true, true, true, false,false },
    // U16   
    .{ false,true, true, true, true, true, true, true, true, true, true, true, true, true, false,false },
    // U32   
    .{ false,false,true, true, true, true, true, true, true, true, true, true, true, true, false,false },
    // U64   
    .{ false,false,false,true, true, true, true, true, true, true, true, true, false,true, false,false },
    // U128  
    .{ false,false,false,false,true, false,false,false,false,false,true, false,false,false,false,false },
    // Usize 
    .{ false,false,false,false,false,true, false,false,false,false,false,true, false,false,false,false },
    // F32   
    .{ false,false,false,false,false,false,false,false,false,false,false,false,true, true, false,false },
    // F64   
    .{ false,false,false,false,false,false,false,false,false,false,false,false,false,true, false,false },
    // Bool  
    .{ true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false },
    // Rune  
    .{ false,false,true, true, true, true, false,false,false,false,false,false,false,false,false,true },
};


//FIXME: span is wrong
fn coerce(self: *Context, span: types.Span, tree: *Ast, left: ast.TypeId, right: ast.TypeId) !bool {
    var lft = left;
    var rit = right;
    var leftty: ast.Type = undefined; 
    var rightty: ast.Type = undefined;
    if (self.type_map.get(left)) |leftt| {
        leftty = leftt;
    } else {
        return false;
    }
    if (self.type_map.get(right)) |rightt| {
        rightty = rightt;
    } else {
        return false;
    }
    if (rightty.base_type == .user and self.contains(rightty.base_type.user.value)) {
        rit = self.get(rightty.base_type.user.value).?.ty.?;
    } else if (rightty.base_type == .user) {
        try self.session.emit(.Error, span, "User type does not exist");
        return error.UndefinedType;
    }
    if (leftty.base_type == .user and self.contains(leftty.base_type.user.value)) {
        lft = self.get(leftty.base_type.user.value).?.ty.?;
    } else if (leftty.base_type == .user) {
        try self.session.emit(.Error, span, "User type does not exist");
        return error.UndefinedType;
    } 
    if (leftty.base_type != .primitive or rightty.base_type != .primitive) return lft == rit;
    if (leftty.base_type.primitive == .Never or rightty.base_type.primitive == .Never) return true;
    if (leftty.base_type.primitive == .Unit and rightty.base_type.primitive == .Unit) return true;


    //const right_idx = @intFromEnum(rightty.base_type.primitive);
    //const left_idx = @intFromEnum(leftty.base_type.primitive);
    //if (right_idx >= coercion_matrix.len or left_idx >= coercion_matrix[0].len) {
    //    return false;
    //}
    //const can_coerce = coercion_matrix[right_idx][left_idx];
    //if (can_coerce) {
    //    const tree_copy = Ast.create(tree.node, tree.span);
    //    const cast = Ast.create(.{ .cast = .{
    //        .expr = try mem.createWith(self.gpa, tree_copy),
    //        .ty = lft,
    //    }}, tree.span);
    //    tree.* = cast;
    //    tree.tyid = lft;
    //}
    _ = tree;

    var mods_match = rit == lft;
    const both_null = leftty.modifiers == null and rightty.modifiers == null;
    const left_mut = leftty.modifiers != null and leftty.modifiers.?[0] == .Mut;
    mods_match = mods_match and both_null or left_mut;
    //both == null and left == right: true
    //left != null and left is mutable and left == right : true
    return mods_match;
}


fn check_type_equality(self: *Context, tree: *Ast, span: types.Span, left: ast.TypeId, right: ast.TypeId) !void {
    if (!try coerce(self, span, tree, left, right)) {
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


