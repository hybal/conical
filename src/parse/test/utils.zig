
const std = @import("std");
const ast = @import("../mod.zig").Ast;
const Ast = ast.Ast;
const common = @import("common");
const diag = @import("diagnostics");
const lex = @import("lex");

ctx: *common.Context,
source: []const u8,
tree: *const Ast,
allocator: std.mem.Allocator,
writer: *std.Io.Writer,

pub fn print_tree(ctx: *common.Context, source: []const u8, tree: *const Ast, allocator: std.mem.Allocator) ![]const u8 {
    var writer = std.Io.Writer.Allocating.init(allocator);
    var self = @This() {
        .ctx = ctx,
        .source = source,
        .tree = tree,
        .allocator = allocator,
        .writer = &writer.writer,
    };
    try self.print_mod();
    for (tree.program.declarations) |node| {
        try self.print_node(node);
    }
    return try writer.toOwnedSlice();
}

fn print_node(self: *@This(), nodeid: ast.AstNodeId) anyerror!void {
    const node = self.tree.get(nodeid);
    switch (node.@"0") {
        .path => try self.print_path(nodeid),
        .terminal => try self.print_terminal(nodeid),
        .terminated => {
            const n: *ast.Terminated = @ptrCast(@alignCast(node.@"1"));
            try self.print_node(n.expr);
            try self.writer.print("; ", .{});
        },
        .item => try self.print_item(nodeid),
        .fn_decl => try self.print_fn_decl(nodeid), 
        .type_decl => try self.print_type_decl(nodeid),
        .var_decl => try self.print_binding(nodeid),
        .import => try self.print_import(nodeid),
        .unit => try self.writer.print("{{}}", .{}),
        .block => try self.print_block(nodeid),
        else => try self.writer.print("Unimplemented<{s}>\n",.{@tagName(node.@"0")}),
    }
}

fn print_block(self: *@This(), nodeid: ast.AstNodeId) !void {
    const node: *ast.Block = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    try self.writer.print("{{ ", .{});
    for (node.exprs) |expr| {
        try self.print_node(expr);
    }
}

fn print_import(self: *@This(), nodeid: ast.AstNodeId) !void {
    const node: *ast.Import = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    try self.writer.print("import ", .{});
    try self.print_path(node.path);
}

fn print_item(self: *@This(), nodeid: ast.AstNodeId) anyerror!void {
    const node: *ast.Item = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    if (node.visibility) |vis| {
        const str = switch(vis.kind) {
            .public => "pub",
        };
        try self.writer.print("{s} ", .{str});
    }
    if (node.linkage) |l| {
        const str = switch (l.kind) {
            .@"export" => "export",
            .@"extern" => "extern",
        };
        try self.writer.print("{s} ", .{str});
    }
    switch (node.item_kind) {
        .function => {
            if (node.function_mods) |fnmods| {
                for (fnmods) |m| {
                    const str = switch(m.kind) {
                        .pure => "pure",
                        .@"inline" => "inline",
                        .@"comptime" => "comptime",
                    };
                    try self.writer.print("{s} ", .{str});
                }
            }
            try self.print_fn_decl(node.item);
        },
        .binding => try self.print_binding(nodeid),
        .@"type" => try self.print_type_decl(nodeid),
    }
}

fn print_type_decl(self: *@This(), nodeid: ast.AstNodeId) !void {
    const node: *ast.TypeDecl = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    try self.writer.print("type ", .{});
    try self.print_ident(node.ident.span);
    try self.print_node(node.ty);
}

fn print_binding(self: *@This(), nodeid: ast.AstNodeId) anyerror!void {
    const node: *ast.VarDecl = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    try self.writer.print("let ", .{});
    if (node.id.modifier) |m| {
        try self.print_binding_mod(m);
    }
    try self.print_ident(node.id.id.span);
    try self.writer.print(" ", .{});
    if (node.ty) |t| {
        try self.writer.print(": ", .{});
        try self.print_node(t);
    }
    try self.writer.print(" = ", .{});
    try self.print_node(node.initialize);
    try self.writer.print("; ", .{});
}

fn print_fn_decl(self: *@This(), nodeid: ast.AstNodeId) anyerror!void {
    const node: *ast.FnDecl = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    std.debug.assert(self.tree.get(nodeid).@"0" == .fn_decl);
    try self.writer.print("fn ", .{});
    try self.print_ident(node.ident.span);
    try self.writer.print("(", .{});
    for (node.generics) |generic| {
        try self.writer.print("$", .{});
        try self.print_ident(generic.ident.span);
        if (generic.expr) |e| {
            try self.print_node(e);
        }
    }
    for (node.params, 0..node.params.len) |p, i| {

        if (p.modifier) |m| {
            try self.print_binding_mod(m);
        }
        try self.print_ident(p.id.span);
        try self.writer.print(": ", .{});
        try self.print_node(node.param_types[i]);
    }
    try self.writer.print(") ", .{});
    if (node.return_ty) |ret| {
        try self.writer.print("-> ", .{});
        try self.print_node(ret);
        try self.writer.print(" ", .{});
    }

    try self.print_node(node.body.?);
}

fn print_binding_mod(self: *@This(), mod: ast.BindingModifier) !void {
    const str = switch (mod.kind) {
        .alias => "alias",
        .move => "move",
        .mut => "mut",
    };
    try self.writer.print("{s}", .{str});
}

fn print_terminal(self: *@This(), nodeid: ast.AstNodeId) !void {
    const node: *ast.Terminal = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    switch (node.termtype) {
        .symbol => |sym| {
            try self.writer.print(".", .{});
            try self.print_ident(sym.span);
        },
        .value => |tok| {
            try self.print_token(tok);
        }
    }
}

fn print_ident(self: *@This(), span: common.Either(common.Span, diag.ErrorId)) !void {
    if (span.is_a()) {
        try self.writer.print("{s}", .{span.a.get_string(self.source)});
    } else {
        try self.writer.print("<ERROR: {}>", .{span.b});
    }

}

fn print_token(self: *@This(), tok: lex.Token) !void {
    try self.writer.print("{s}", .{tok.to_string(self.source)});
}

fn print_mod(self: *@This()) !void {
    try self.writer.print("mod ", .{});
    try self.print_path(self.tree.program.module.?.path);
    try self.writer.print("; ", .{});
}

fn print_path(self: *@This(), pathid: ast.AstNodeId) !void {
    const path: *ast.Path = @ptrCast(@alignCast(self.tree.get(pathid).@"1"));
    for (path.parts, 0..path.parts.len) |part, i| {
        try self.print_ident(part.span);
        if (i < path.parts.len - 1) {
            try self.writer.print("::", .{});
        }
    }
}


