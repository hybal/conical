const std = @import("std");
const lex = @import("lexer.zig");
const ast = @import("Ast.zig");
const parse = @import("parser.zig");

const source = 
        \\ a ++= 1
    ;

fn print_tree(node: ?*ast.Ast, prefix: []const u8, is_left: bool) void {
    if (node == null) return;

    const new_prefix = if (!is_left) std.fmt.allocPrint(std.heap.page_allocator, "{s}│   ", .{prefix}) catch prefix
                      else std.fmt.allocPrint(std.heap.page_allocator, "{s}    ", .{prefix}) catch prefix;

    if (!is_left) {
        std.debug.print("{s}├── {s}\n", .{ prefix, source[node.?.token.start..node.?.token.end] });
    } else {
        std.debug.print("{s}└── {s}\n", .{ prefix, source[node.?.token.start..node.?.token.end] });
    }

    // Print right subtree first so it appears on top
    print_tree(node.?.right, new_prefix, false);
    print_tree(node.?.left, new_prefix, true);
}

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    var parser = parse.init_from_source(source, gpa.allocator());
    print_tree(try parser.parse(), "", false);

}


