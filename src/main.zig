const std = @import("std");
const lex = @import("lexer.zig");
const ast = @import("Ast.zig");
const parse = @import("parser.zig");

const source =
    \\ if a == 1 { 1 } else if a == 3 {2}
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
        .block => |blk| {
            std.debug.print("{{\n", .{});
            for (blk.exprs) |expr| {
                std.debug.print("    ", .{});
                print_tree(expr);
                std.debug.print("\n", .{});
            }
            std.debug.print("}}\n", .{});
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
        .if_stmt => |stmt| {
            std.debug.print("if ", .{});
            print_tree(stmt.condition);
            print_tree(stmt.block);
            if (stmt.else_block) |eblk| {
                std.debug.print("else ", .{});
                print_tree(eblk);
            }
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
