const std = @import("std");
const lex = @import("lexer.zig");
const ast = @import("Ast.zig");
const parse = @import("parser.zig");

const source =
    \\ a = 1;
;
fn print_tree(node: ?*ast.Ast) void {
    if (node == null) return;
    switch (node.?.*) {
        .expr => |expr| {
            print_tree(expr.left);
            std.debug.print(" {s} ", .{source[expr.token.start..expr.token.end]});
            print_tree(expr.right);
            if (expr.terminated != null) {
                std.debug.print(";", .{});
            }
        },
        .ternary => |expr| {
            print_tree(expr.condition);
            std.debug.print(" ? ", .{});
            print_tree(expr.true_path);
            std.debug.print(" : ", .{});
            print_tree(expr.false_path);
        },
        .assignment => |expr| {
            std.debug.print(" {s} ", .{source[expr.id.start..expr.id.end]});
            std.debug.print(" {s} ", .{source[expr.op.start..expr.op.end]});
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
