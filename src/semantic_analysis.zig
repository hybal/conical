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
    fn get(self: *@This(), key: []const u8) ?types.Symbol {
        if (self.symtab.items.len == 0) return null;
        for (0..self.symtab.items.len) |i| {
            if (self.symtab.items[self.symtab.items.len - 1 - i].get(key)) |val| {
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
        try resolve_local(self, tree);
        main.print_type(try type_check(self, tree));
        std.debug.print("\n", .{});
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


fn resolve_local(self: *Context, tree: *Ast) !void {
    switch (tree.node) {
        .var_decl => |*decl| {
            if (self.at_global) {
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
                const prev_in_global = self.at_global;
                self.at_global = false;
                try resolve_local(self, body);
                self.in_func = prev_in_func;
                self.at_global = prev_in_global;
            }
            if (self.at_global) return;
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
        .fn_call => |expr| {
            try resolve_local(self, expr.func);
            for (expr.args) |fnc| {
                try resolve_local(self, fnc);
            }
        },
        else => |val| {
            std.debug.print("Unsupported operation: {s}\n", .{@tagName(val)});
            return error.UnsupportedOperation;
}
    }
}

//TODO: Do type coercian
fn type_check(self: *Context, tree: *Ast) !ast.Type {
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
                    const val = self.get(term.span.get_string(self.source));
                    if (val == null) {
                        std.debug.print("Unknown Identifier: {s}\n", .{term.span.get_string(self.source)});
                        return error.UnknownIdentifier;
                    }
                    
                    const nval = val.?;
                    if (nval.ty == null) {
                        _ = try type_check(self, nval.ast);
                    }
                    out_type = nval.ty.?;
                },
                else => {}
            }
            return out_type;
        },
        .terminated => |expr| {
            _ = try type_check(self, expr);
            return ast.Type.unit;
        },
        .binary_expr => |expr| {
            const left = try type_check(self, expr.left);
            const right = try type_check(self, expr.right);
            switch (expr.op.tag) {
                .plus, .minus, .slash, .star, .caret, .percent,
                .pipe, .amp, .shl, .shr => {
                    try check_type_equality(self, left, right);
                    return left;
                },
                .eq2, .noteq, .lt, .lteq, .gt, .gteq => {
                    try check_type_equality(self, left, right);
                    return ast.Type.Bool;
                },
                .pipe2, .amp2 => {
                    try check_type_equality(self, ast.Type.Bool, left);
                    return ast.Type.Bool;
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
                try check_type_equality(self, decl.return_ty, ret);
            }
            const out = self.get(decl.ident.span.get_string(self.source));
            if (out) |t| {
                return t.ty.?;
            }
            return error.UnknownIdentifier;
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
        .ternary => |expr| {
            var bol = ast.Type.unit;
            bol.base_type.primitive = .Bool;
            try check_type_equality(self, try type_check(self, expr.condition), bol);
            const out = try type_check(self, expr.true_path);
            try check_type_equality(self,
                out,
                try type_check(self, expr.false_path)
            );
            return out;
        },
        .var_decl => |stmt| {
            if (stmt.ty == null and stmt.initialize == null) {
                try self.session.emit(.Error, tree.span, "Variable declerations without initializations must have explicit types");
                return error.UntypedVariable;
            }
            var ty = stmt.ty;
            if (ty == null) {
                ty = try type_check(self, stmt.initialize.?);
            } else if (stmt.initialize) |initt| {
                try check_type_equality(self,
                    ty.?,
                    try type_check(self, initt)
                );
            }
            var sym = self.getScope().?.getPtr(stmt.ident.span.get_string(self.source)) orelse return error.UnknownIdentifier;
            sym.ty = ty;
            
            return ast.Type.unit;

        },
        .fn_call => |expr| {
            const left = try type_check(self, expr.func);
            if (left.base_type != .func) {
                try self.session.emit(.Error, tree.span, "Attempt to call a non-function type");
                return error.NonFunctionCall;
            }
            return left.base_type.func.ret.*;

        },
        else => unreachable
    }
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
            std.debug.print("Expected Type: {s}, got: {s}\n", .{@tagName(left.base_type.primitive), @tagName(right.base_type.primitive)});
            return error.TypeMismatch;
        }
    }

}


