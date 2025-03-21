const std = @import("std");
const lex = @import("lexer.zig");
const ast = @import("Ast.zig");
const parse = @import("parser.zig");

const source =
    \\ mut a = 1 + 4
    ;


fn print_type(ty: ast.Type) void {
    if (ty.modifiers) |mods| {
        for (mods) |mod| {
            std.debug.print("{s}", .{mod.get_string(source)});
        }
    }
    switch (ty.base_type) {
        .primitive => |val| {
            std.debug.print(" prim<{s}>", .{val.get_string()});
        },
        .user => |val| {
            std.debug.print(" {s}", .{val.span.get_string(source)});
        }
    }
}
fn print_tree(node: ?*ast.Ast) void {
    if (node == null) return;
    switch (node.?.*) {
        .terminal => |term| {
            std.debug.print(" {s}", .{term.span.get_string(source)});
        },
        .binary_expr => |expr| {
            print_tree(expr.left);
            std.debug.print(" {s}", .{expr.op.span.get_string(source)});
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
        .var_decl => |decl| {
            std.debug.print(" {s}", .{if (decl.is_mut) "mut" else "let"});
            std.debug.print(" {s}", .{decl.ident.span.get_string(source)});
            if (decl.ty) |ty| {
                std.debug.print(": ", .{});
                print_type(ty);
            }
            if (decl.initialize) |init| {
                print_tree(init);
            }
        },
        .assignment => |expr| {
            std.debug.print(" {s}", .{expr.op.span.get_string(source)});
            print_tree(expr.expr);
        },
        .if_stmt => |stmt| {
            std.debug.print("if", .{});
            print_tree(stmt.condition);
            print_tree(stmt.block);
            if (stmt.else_block) |eblk| {
                std.debug.print("else", .{});
                print_tree(eblk);
            }
        },
        else => |thing| std.debug.print("unkown: {any}", .{thing}),
    }
}


pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    var parser = parse.init_from_source(source, gpa.allocator());
    print_tree(try parser.parse());
    std.debug.print("\n", .{});
}
