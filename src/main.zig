const std = @import("std");
const lex = @import("lexer.zig");
const ast = @import("Ast.zig");
const parse = @import("parser.zig");
const sema = @import("semantic_analysis.zig");
const types = @import("types.zig");
const diag = @import("diag.zig");


var source: []const u8 = "let a = 1;";

fn print_type(ty: ast.Type) void {
    if (ty.modifiers) |mods| {
        for (mods) |mod| {
            std.debug.print("{s} ", .{mod.get_string(source)});
        }
    }
    switch (ty.base_type) {
        .primitive => |val| {
            std.debug.print("prim<{s}> ", .{val.get_string()});
        },
        .user => |val| {
            std.debug.print("{s} ", .{val.span.get_string(source)});
        }
    }
}
fn print_tree(node: ?*ast.Ast) void {
    if (node == null) return;
    switch (node.?.node) {
        .terminal => |term| {
            std.debug.print("{s} ", .{term.span.get_string(source)});
        },
        .binary_expr => |expr| {
            print_tree(expr.left);
            std.debug.print("{s} ", .{expr.op.span.get_string(source)});
            print_tree(expr.right);
        },
        .unary_expr => |expr| {
            std.debug.print("{s} ", .{expr.op.span.get_string(source)});
            print_tree(expr.expr);
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
            std.debug.print("{s} ", .{if (decl.is_mut) "mut" else "let"});
            std.debug.print("{s} ", .{decl.ident.span.get_string(source)});
            if (decl.ty) |ty| {
                std.debug.print(": ", .{});
                print_type(ty);
            }
            if (decl.initialize) |init| {
                std.debug.print("= ", .{});
                print_tree(init);
                std.debug.print(";\n", .{});
            }
        },
        .assignment => |expr| {
            if (expr.lvalue.derefs > 0) {
                for (0..expr.lvalue.derefs) |_| {
                    std.debug.print("*", .{});
                }
            }
            std.debug.print("{s}", .{expr.lvalue.ident.span.get_string(source)});
            if (expr.lvalue.array_access) |arrs| {
                for (arrs) |arr| {
                    std.debug.print("[", .{});
                    print_tree(arr);
                    std.debug.print("]", .{});
                }
            }
            std.debug.print(" {s} ", .{expr.op.span.get_string(source)});
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
        .while_loop => |loop| {
            std.debug.print("while ", .{});
            print_tree(loop.condition);
            print_tree(loop.block);
        },
        .fn_decl => |fnc| {
            std.debug.print("fn ", .{});
            std.debug.print("{s} ", .{fnc.ident.span.get_string(source)});
            std.debug.print("(", .{});
            for (fnc.params) |param| {
                std.debug.print("{s}, ", .{ param.span.get_string(source) });
            }
            std.debug.print("): (", .{});
            for (fnc.param_types) |ty| {
                print_type(ty);
                std.debug.print(", ", .{});
            }
            std.debug.print(") -> ", .{});
            print_type(fnc.return_ty);
            print_tree(fnc.body);
        },
        .terminated => |expr| {
            print_tree(expr);
            std.debug.print("; ", .{});
        },
        else => |thing| std.debug.print("unkown: {any}\n", .{thing}),
    }
}




pub fn main() !u8 {
    var child: std.heap.GeneralPurposeAllocator(.{}) = .init;
    var gpa = std.heap.ArenaAllocator.init(child.allocator());
    const args = try std.process.argsAlloc(gpa.allocator());
    defer std.process.argsFree(gpa.allocator(), args);

    if (args.len < 2) {
        return error.NotEnoughCmdlineArgs;
    }

    const file = try std.fs.cwd().openFile(args[1], .{});
    defer file.close();

    source = try file.readToEndAlloc(gpa.allocator(), std.math.maxInt(usize));
    var session = diag.Session.init(gpa.allocator(), source);
    var parser = parse.init_from_source(source, &session, gpa.allocator());
    var context = try sema.init_context(source, &session, gpa.allocator());
    const trees = parser.parse() catch {
        try session.flush(std.io.getStdErr().writer());
        return 1;
    };
    for (trees) |tree| {
        print_tree(tree);
        std.debug.print("\n", .{});
    }

    sema.resolve(&context, trees) catch {
        try session.flush(std.io.getStdErr().writer());
        return 1;
    };
    return 0;
}
