const std = @import("std");
const ast = @import("Ast.zig");
const Ast = ast.Ast;
const main = @import("main.zig");
const diag = @import("diag.zig");
const types = @import("types.zig");
const mem = @import("mem.zig");

pub fn init_context(source: []const u8, session: *diag.Session, gpa: std.mem.Allocator) !Context {
    return .{
        .symtab = .init(gpa),
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
    scope: usize,
    source: []const u8,
    gpa: std.mem.Allocator,
    in_func: bool,
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
                        .ty = decl.ty,
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
                    var args = std.ArrayList(ast.Type).init(self.gpa);
                    for (decl.param_types) |ty| {
                        try args.append(ty);
                    }
                    try self.push(decl.ident.span.get_string(self.source), .{
                        .ty = .{
                            .base_type = .{
                                .func = .{
                                    .args = try args.toOwnedSlice(),
                                    .ret = try mem.createWith(self.gpa, decl.return_ty),
                                },
                            },
                            .modifiers = null
                        },
                        .ident = decl.ident.span,
                        .ast = tree
                    });
                } else {
                    try self.session.emit(.Error, tree.span, "Function Shadows Previous Decleration");
                    return error.FunctionShadowsPreviousDecleration;
                }
            },
            else => |_| {
                try self.session.emit(.Error, tree.span, "Invalid operation at the global scope");
                return error.UnallowedOperation;
            }
        }
    }
}


//TODO: Do type coercian
fn analyze(self: *Context, tree: *Ast) anyerror!ast.Type {
    switch (tree.node) {
        .terminal => |term| {
            var out_type: ast.Type = ast.Type.unit;
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
                    out_type = nval.ty.?;
                },
                else => {}
            }
            return out_type;
        },
        .terminated => |expr| {
            _ = try analyze(self, expr);
            return ast.Type.unit;
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
                    return ast.Type.Bool;
                },
                .pipe2, .amp2 => {
                    try check_type_equality(self, tree.span, ast.Type.Bool, left);
                    return ast.Type.Bool;
                },
                else => unreachable
            }
        },
        .unary_expr => |expr| {
            var ty = try analyze(self, expr.expr);
            switch (expr.op.tag) {
                .minus, .tilde => return ty,
                .amp => {
                    var mods: std.ArrayList(ast.TypeModifier) = undefined;
                    if (ty.modifiers) |modd| {
                        mods = .fromOwnedSlice(self.gpa, modd);
                    } else {
                        mods = .init(self.gpa);
                    }
                    try mods.append(.Ref);
                    ty.modifiers = try mods.toOwnedSlice();
                    return ty;
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
                    return ty;
                },
                .bang => {
                    return ast.Type.Bool;
                },
                else => unreachable
            }
        },
        .fn_decl => |decl| {
            if (!self.at_global) {
                if (!self.contains(decl.ident.span.get_string(self.source))) {
                    var args = std.ArrayList(ast.Type).init(self.gpa);
                    for (decl.param_types) |ty| {
                        try args.append(ty);
                    }
                    try self.push(decl.ident.span.get_string(self.source), .{
                        .ty = .{
                            .base_type = .{
                                .func = .{
                                    .args = try args.toOwnedSlice(),
                                    .ret = try mem.createWith(self.gpa, decl.return_ty),
                                },
                                },
                            .modifiers = null
                        },
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
                self.in_func = true;
                const prev_in_global = self.at_global;
                self.at_global = false;
                const ret_ty = try analyze(self, body);
                try check_type_equality(self, tree.span, decl.return_ty, ret_ty);
                self.in_func = prev_in_func;
                self.at_global = prev_in_global;
                _ = self.symtab.pop();
                return ast.Type.unit;
            }
            return error.Unknown;
        },
        .block => |expr| {
            if (expr.exprs.len == 0) {
                return ast.Type.unit;
            }
            try self.symtab.append(types.SymTab.init(self.gpa));
            for(expr.exprs[0..expr.exprs.len - 1]) |exp| {
                try check_type_equality(self, tree.span, ast.Type.unit, try analyze(self, exp) );
            }
            const out = try analyze(self, expr.exprs[expr.exprs.len - 1]);
            _ = self.symtab.pop();
            return out;
        },
        .assignment => |stmt| {
            const lvalue = try analyze(self, stmt.lvalue);
            const assigned = try analyze(self, stmt.expr);
            try check_type_equality(self, tree.span, lvalue, assigned);
            return ast.Type.unit;
        },
        .if_stmt => |stmt| {
            const cond = try analyze(self, stmt.condition);
            var bol = ast.Type.unit;
            bol.base_type.primitive = .Bool;
            try check_type_equality(self, tree.span, cond, bol);
            const blk = try analyze(self, stmt.block);
            if (stmt.else_block == null) return ast.Type.unit;
            const else_block = stmt.else_block.?;
            try check_type_equality(self, tree.span, blk, try analyze(self, else_block));
            return blk;
        },
        .while_loop => |stmt| {
            const cond = try analyze(self, stmt.condition);
            var bol = ast.Type.unit;
            bol.base_type.primitive = .Bool;
            try check_type_equality(self, tree.span, cond, bol);
            const blk = try analyze(self, stmt.block);
            try check_type_equality(self, tree.span, blk, ast.Type.unit);
            return ast.Type.unit;
        },
        .ternary => |expr| {
            var bol = ast.Type.unit;
            bol.base_type.primitive = .Bool;
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
            return ast.Type.unit;
        },
        .fn_call => |expr| {
            const left = try analyze(self, expr.func);
            if (left.base_type != .func) {
                try self.session.emit(.Error, expr.func.span, "Attempt to call a non-function type");
                return error.NonFunctionCall;
            }
            return left.base_type.func.ret.*;

        },
        else => unreachable
    }
}


fn check_type_equality(self: *Context, span: types.Span, left: ast.Type, right: ast.Type) !void {
    var lhash: u64 = 0;
    var rhash: u64 = 0;
    if (left.chash == null) {
        lhash = left.hash();
    } else {
        lhash = left.chash.?;
    }
    if (right.chash == null) {
        rhash = right.hash();
    } else {
        rhash = right.chash.?;
    }

    if (lhash != rhash) {
        const msg = try std.fmt.allocPrint(self.gpa,
            "Expected Type: {s}, got: {any}\n", 
            .{@tagName(left.base_type.primitive), right}
        );
        try self.session.emit(.Error, span, msg);
        return error.TypeMismatch;
    }
}


