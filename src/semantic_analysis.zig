const std = @import("std");
const ast = @import("Ast.zig");
const Ast = ast.Ast;

const diag = @import("diag.zig");
const types = @import("types.zig");

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
    fn get(self: *@This(), key: []const u8) ?types.Symbol {
        if (self.symtab.items.len == 0) return null;
        return self.symtab.getLast().get(key);
    }
};


pub fn resolve(self: *Context, trees: []*Ast) !void {
    try resolve_global(self, trees);
    for (trees) |tree| {
        try resolve_local(self, tree);
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
                    try self.push(decl.ident.span.get_string(self.source), .{
                        .ty = decl.return_ty,
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


fn resolve_local(self: *Context, tree: *Ast) !void {
    switch (tree.node) {
        .var_decl => |*decl| {
            if (self.at_global) {
                if (decl.initialize) |init| {
                    try resolve_local(self, init);
                    if (decl.ty) |ty| {
                        try check_type_equality(self, try type_check(self, init), ty);
                    } else {
                        decl.ty = try type_check(self, init);
                    }
                }
                return;
            }
            if (!self.contains(decl.ident.span.get_string(self.source))) {
                try self.push(decl.ident.span.get_string(self.source), .{
                    .ty = decl.ty,
                    .ident = decl.ident.span,
                    .ast = tree
                });
            } else {
                return error.VariableShadowsPreviousDecleration;
            }
        }, 
        .fn_decl => |decl| {
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
                try resolve_local(self, body);
                try check_type_equality(self, decl.return_ty, try type_check(self, body));
                self.in_func = prev_in_func;
            }
            if (self.at_global) return;
            if (!self.contains(decl.ident.span.get_string(self.source))) {
                try self.push(decl.ident.span.get_string(self.source), .{
.ty = null,
                    .ident = decl.ident.span,
                    .ast = tree
                });
            } else {
                    std.debug.print("Function Shadows Previous Decleration: {s}\n", .{decl.ident.span.get_string(self.source)});
                return error.FunctionShadowsPreviousDecleration;
            }
        },
        .terminal => |term| {
            if (term.tag == .ident) {
                if (!self.contains(term.span.get_string(self.source))) {
                    std.debug.print("Unknown Identifier: {s}\n", .{term.span.get_string(self.source)});
                    return error.UnknownIdentifier;
                }
            }
        },
        .binary_expr => |expr| {
            try resolve_local(self, expr.left);
            try resolve_local(self, expr.right);
        },
        .unary_expr => |expr| {
            try resolve_local(self, expr.expr);
        },
        .assignment => |expr| {
            const prev = self.in_assignment;
            self.in_assignment = true;
            try resolve_local(self, expr.lvalue);
            try resolve_local(self, expr.expr);
            self.in_assignment = prev;
        },
        .block => |block| {
            for (block.exprs) |expr| {
                try resolve_local(self, expr);
            }
        },
        .terminated => |expr| {
            try resolve_local(self, expr);
        },
        .ternary => |expr| {
            try resolve_local(self, expr.condition);
            try resolve_local(self, expr.true_path);
            try resolve_local(self, expr.false_path);
        },
        .if_stmt => |stmt| {
            try resolve_local(self, stmt.condition);
            try resolve_local(self, stmt.block);
            if (stmt.else_block) |else_block| {
                try resolve_local(self, else_block);
            }
        },
        .while_loop => |stmt| {
            try resolve_local(self, stmt.condition);
            const in_loop_prev = self.in_loop;
            self.in_loop = true;
            try resolve_local(self, stmt.block);
            self.in_loop = in_loop_prev;
        },
        else => |val| {
            std.debug.print("Unsupported operation: {s}\n", .{@tagName(val)});
            return error.UnsupportedOperation;
        }
    }
}

//TODO: Check that operators work with the types
//TODO: Do type coercian
fn type_check(self: *Context, tree: *Ast) !ast.Type {
    switch (tree.node) {
        .terminal => |term| {
            var out_type: ast.Type = .{ .base_type = .{ .primitive = .Unit }, .modifiers = null };
            var modifiers = std.ArrayList(ast.TypeModifier).init(self.gpa);
            switch (term.tag) {
                .int_literal => out_type.base_type = .{ .primitive = .I32 }, //TODO: automatically widen the type to acomodate a larger literal / automatically determine sign
                .float_literal => out_type.base_type = .{ .primitive = .F32 },
                .string_literal, .raw_string_literal => {
                    out_type.base_type = .{ .primitive = .U8 };
                    try modifiers.append(.Slice);
                },
                .char_literal => out_type.base_type = .{ .primitive = .U8 },
                .keyword_true, .keyword_false => out_type.base_type = .{ .primitive = .Bool },
                else => {}
            }
            out_type.modifiers = try modifiers.toOwnedSlice();
            return out_type;
        },
        .terminated => |expr| {
            _ = try type_check(self, expr);
            return ast.Type.unit;
        },
        .binary_expr => |expr| {
            const left = try type_check(self, expr.left);
            const right = try type_check(self, expr.right);
            try check_type_equality(self, left, right);
            switch (expr.op.tag) {
                .plus, .minus, .slash, .star, .caret, .percent,
                .pipe, .amp, .shl, .shr => {
                    return left;
                },
                .pipe2, .amp2, .eq2, .noteq, .lt, .lteq, .gt, .gteq => {
                    var out = ast.Type.unit;
                    out.base_type.primitive = .Bool;
                    return out;
                },
                else => unreachable
            }
        },
        .unary_expr => |expr| {
            const ty = try type_check(self, expr.expr);
            switch (expr.op.tag) {
                .minus, .tilde, .star, .amp => return ty,
                .bang => {
                    var out = ast.Type.unit;
                    out.base_type.primitive = .Bool;
                    return out;
                },
                else => unreachable
            }
        },
        .fn_decl => |decl| {
            if (decl.body) |body| {
                const ret = try type_check(self, body);
                try check_type_equality(self, ret, decl.return_ty);
            }
            return decl.return_ty;
        },
        .block => |expr| {
            for(expr.exprs[0..expr.exprs.len - 1]) |exp| {
                try check_type_equality(self, try type_check(self, exp), ast.Type.unit);
            }
            return try type_check(self, expr.exprs[expr.exprs.len - 1]);
        },
        .assignment => |stmt| {
            const lvalue = try type_check(self, stmt.lvalue);
            const assigned = try type_check(self, stmt.expr);
            try check_type_equality(self, lvalue, assigned);
            return ast.Type.unit;
        },
        .if_stmt => |stmt| {
            const cond = try type_check(self, stmt.condition);
            var bol = ast.Type.unit;
            bol.base_type.primitive = .Bool;
            try check_type_equality(self, cond, bol);
            const blk = try type_check(self, stmt.block);
            if (stmt.else_block == null) return ast.Type.unit;
            const else_block = stmt.else_block.?;
            try check_type_equality(self, blk, try type_check(self, else_block));
            return blk;
        },
        .while_loop => |stmt| {
            const cond = try type_check(self, stmt.condition);
            var bol = ast.Type.unit;
            bol.base_type.primitive = .Bool;
            try check_type_equality(self, cond, bol);
            const blk = try type_check(self, stmt.block);
            try check_type_equality(self, blk, ast.Type.unit);
            return ast.Type.unit;
        },
        else => {}
    }
    unreachable;
}


fn check_type_equality(self: *Context, left: ast.Type, right: ast.Type) !void {
    if (left.base_type == .user) {
        if (right.base_type == .primitive) {
            return error.TypeMismatch;
        }
        const rid = right.base_type.user.span;
        const lid = left.base_type.user.span;
        if (rid.start != lid.start or rid.end != lid.end) {
            std.debug.print("Expected Type: {s}, got: {s}\n", .{rid.get_string(self.source), lid.get_string(self.source)});
            return error.TypeMismatch;
        }
        return error.OperatorNotDefinedBetweenUserTypes; 
    } else {
        if (right.base_type != .primitive) {
            return error.TypeMismatch;
        }
        if (left.base_type.primitive != right.base_type.primitive) {
            std.debug.print("Expected Type: {any}, got: {any}\n", .{left.base_type.primitive, right.base_type.primitive});
            return error.TypeMismatch;
        }
    }

}


