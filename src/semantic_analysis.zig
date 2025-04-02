const std = @import("std");
const ast = @import("Ast.zig");
const Ast = ast.Ast;

const types = @import("types.zig");

pub fn init_context(source: []const u8, gpa: std.mem.Allocator) !Context {
    return .{
        .symtab = .init(gpa),
        .scope = 0,
        .source = source,
        .gpa = gpa,
        .in_func = false,
        .in_condition = false,
        .in_loop = false,
        .at_global = true,
    };
}

pub const Context = struct {
    symtab: std.ArrayList(types.SymTab),
    scope: usize,
    source: []const u8,
    gpa: std.mem.Allocator,
    in_func: bool,
    in_condition: bool,
    in_loop: bool,
    at_global: bool,
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


fn resolve_global(self: *Context, trees: []*Ast) !void {
    try self.symtab.append(types.SymTab.init(self.gpa));
    for (trees) |tree| {
        switch (tree.*) {
            .var_decl => |decl| {
                if (!self.contains(decl.ident.span.get_string(self.source))) {
                    try self.push(decl.ident.span.get_string(self.source), .{
                        .ty = null,
                        .ident = decl.ident.span,
                        .ast = tree
                    });
                } else {
                    return error.VariableShadowsPreviousDecleration;
                }
                //TODO: resolve assignment expression
            },
            .fn_decl => |decl| {
                if (!self.contains(decl.ident.span.get_string(self.source))) {
                    try self.push(decl.ident.span.get_string(self.source), .{
                        .ty = null,
                        .ident = decl.ident.span,
                        .ast = tree
                    });
                } else {
                    std.debug.print("Function Shadows Previous Decleration: {s}", .{decl.ident.span.get_string(self.source)});
                    return error.FunctionShadowsPreviousDecleration;
                }
            },
            else => |val| {
                std.debug.print("'{s}' is not allowed at a global scope.\n",.{@tagName(val)});
            }
        }
    }
}

pub fn resolve(self: *Context, trees: []*Ast) !void {
    try resolve_global(self, trees);
    for (trees) |tree| {
        try resolve_local(self, tree);
    }
}

fn resolve_local(self: *Context, tree: *Ast) !void {
    switch (tree.*) {
        .var_decl => |decl| {
            if (self.at_global) {
                if (decl.initialize) |init| {
                    try resolve_local(self, init);
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
            if (!self.contains(expr.lvalue.ident.span.get_string(self.source))) {
                return error.UnknownIdentifier;
            }
            if (self.get(expr.lvalue.ident.span.get_string(self.source))) |sym| {
                switch (sym.ast.*) {
                    .var_decl => |decl| {
                        if (!decl.is_mut) {
                            return error.AssignmentToImmutableVariable;
                        }
                    },
                    else => {}
                }
            }
        },
        .block => |block| {
            for (block.exprs) |expr| {
                try resolve_local(self, expr);
            }
        },
        .optional_block => |block| {
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






