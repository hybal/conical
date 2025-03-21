const std = @import("std");
const lex = @import("lexer.zig");
const ast = @import("Ast.zig");
const parse = @import("parser.zig");

const source =
    \\ a = 1
;

fn print_tree(node: ?*ast.Ast) void {
    if (node == null) return;
    switch (node.?.*) {
        .terminal => |term| {
            std.debug.print(" {s} ", .{term.get_token_string(source)});
        },
        .binary_expr => |expr| {
            print_tree(expr.left);
            std.debug.print(" {s} ", .{expr.op.get_token_string(source)});
            print_tree(expr.right);
        },
        .ternary => |expr| {
            print_tree(expr.condition);
            std.debug.print(" ? ", .{});
            print_tree(expr.true_path);
            std.debug.print(" : ", .{});
            print_tree(expr.false_path);
        },
        .assignment => |expr| {
            std.debug.print(" {s} ", .{expr.id.get_token_string(source)});
            std.debug.print(" {s} ", .{expr.op.get_token_string(source)});
            print_tree(expr.expr);
        },
        else => std.debug.print("unkown", .{}),
    }
}


pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    var parser = parse.init_from_source(source, gpa.allocator());
    print_tree(try parser.parse());
    std.debug.print("\n", .{});
}
