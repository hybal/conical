const std = @import("std");
const ast = @import("Ast.zig");
const Ast = ast.Ast;

pub fn ast_to_json(tree: *const Ast) !void {
    const fmt = std.json.fmt(tree.*, .{});
    try fmt.format(&std.testing.io_instance.stderr_writer.interface);
}


