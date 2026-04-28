const std = @import("std");
const ast = @import("Ast.zig");
const common = @import("common");
const lex = @import("lex");
const diag = @import("diagnostics");

ctx: *common.Context,
tree: *ast.Ast,
source: []const u8,
writer: *std.Io.Writer,

pub fn print(ctx: *common.Context, tree: *ast.Ast, source: []const u8, writer: *std.Io.Writer) !void {
    var self = @This() {
        .ctx = ctx,
        .tree = tree,
        .source = source,
        .writer = writer,
    };

}

fn get_source_string(self: *@This(), span: common.Either(common.Span, diag.ErrorId)) []const u8 {
    if (span.is_a()) {
        return span.a.get_string(self.source);
    }
    return "<ERROR>";
}

fn print_token(self: *@This(), tok: lex.Token) []const u8 {
    return tok.to_string(self.source);
}

fn print_single(self: *@This(), nodeid: ast.AstNodeId) !void {
    const kind, const node = self.tree.get(nodeid);
    switch (kind) {
        .path => {
            const n: *ast.Path = @ptrCast(@alignCast(node));
            try self.writer.print("{s}", .{n.parts[0]});
            for (n.parts[1..]) |p| {
                try self.writer.print("::", .{});
                try self.writer.print("{s}", .{self.get_source_string(p.span)});
            }
        },
        .terminal => {
            const n: *ast.Terminal = @ptrCast(@alignCast(node));
            switch (n.termtype) {
                .symbol => |id| try self.writer.print(".{s}", .{self.get_source_string(id.span)}),
                .value => |tok| try self.writer.print("{s}", .{self.print_token(tok)}),
            }
        },
        .terminated => {
            const n: *ast.Terminated = @ptrCast(@alignCast(node));
            try self.print_single(n.expr);
        },
        .unit => {
            try self.writer.print("()", .{});
        },
    }
}
