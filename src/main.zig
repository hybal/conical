const std = @import("std");
const lex = @import("lexer.zig");
const ast = @import("Ast.zig");
const parse = @import("parser.zig");
const sema = @import("semantic_analysis.zig");
const types = @import("types.zig");
const diag = @import("diag.zig");


var source: []const u8 = "let a = 1;";

pub fn print_type(type_map: *types.TypeTbl, tyid: ast.TypeId) void {
    const ty = type_map.get(tyid).?;
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
        },
        .strct => |strct| {
            std.debug.print("struct {{ ", .{});
            var iter = strct.fields.iterator();
            while (iter.next()) |val| {
                std.debug.print("{s}: ", .{val.key_ptr.*});
                std.debug.print("{s}; ", .{type_map.get(val.value_ptr.*).?.get_string(type_map, std.heap.page_allocator, source) catch "err"});

            }
            std.debug.print("}}", .{});
        },
        .@"enum" => |enm| {
            std.debug.print("{s}", .{
                enm.get_string(std.heap.page_allocator, type_map, source)
                    catch "err",
            });
        },
        .func => |func| {
            std.debug.print("(", .{});
            for (func.args) |arg| {
                print_type(type_map, arg);
                std.debug.print(",", .{});
            }
            std.debug.print(") -> ", .{});
            print_type(type_map, func.ret);
        },
        .@"type" => |tyyid| {
            print_type(type_map, tyyid);
        },
    }
}
fn print_tree(type_map: *types.TypeTbl, node: ?*ast.Ast) void {
    if (node == null) return;
    switch (node.?.node) {
        .terminal => |term| {
            std.debug.print("{s} ", .{term.span.get_string(source)});
        },
        .binary_expr => |expr| {
            print_tree(type_map, expr.left);
            std.debug.print("{s} ", .{expr.op.span.get_string(source)});
            print_tree(type_map, expr.right);
        },
        .unary_expr => |expr| {
            std.debug.print("{s} ", .{expr.op.span.get_string(source)});
            print_tree(type_map, expr.expr);
        },
        .block => |blk| {
            std.debug.print("{{\n", .{});
            for (blk.exprs) |expr| {
                std.debug.print("    ", .{});
                print_tree(type_map, expr);
                std.debug.print("\n", .{});
            }
            std.debug.print("}}\n", .{});
        },
        .ternary => |expr| {
            print_tree(type_map, expr.condition);
            std.debug.print(" ? ", .{});
            print_tree(type_map, expr.true_path);
            std.debug.print(" : ", .{});
            print_tree(type_map, expr.false_path);
        },
        .var_decl => |decl| {
            std.debug.print("{s} ", .{if (decl.is_mut) "mut" else "let"});
            std.debug.print("{s} ", .{decl.ident.span.get_string(source)});
            if (decl.ty) |ty| {
                std.debug.print(": ", .{});
                print_type(type_map, ty);
            }
            if (decl.initialize) |init| {
                std.debug.print("= ", .{});
                print_tree(type_map, init);
                std.debug.print(";\n", .{});
            }
        },
        .assignment => |expr| {
            print_tree(type_map, expr.lvalue);
            std.debug.print(" {s} ", .{expr.op.span.get_string(source)});
            print_tree(type_map, expr.expr);
        },
        .if_stmt => |stmt| {
            std.debug.print("if ", .{});
            print_tree(type_map, stmt.condition);
            print_tree(type_map, stmt.block);
            if (stmt.else_block) |eblk| {
                std.debug.print("else ", .{});
                print_tree(type_map, eblk);
            }
        },
        .while_loop => |loop| {
            std.debug.print("while ", .{});
            print_tree(type_map, loop.condition);
            print_tree(type_map, loop.block);
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
                print_type(type_map, ty);
                std.debug.print(", ", .{});
            }
            std.debug.print(") -> ", .{});
            print_type(type_map, fnc.return_ty);
            print_tree(type_map, fnc.body);
        },
        .terminated => |expr| {
            print_tree(type_map, expr);
            std.debug.print("; ", .{});
        },
        .fn_call => |expr| {
            print_tree(type_map, expr.func);
            std.debug.print("(", .{});
            for (expr.args) |arg| {
                print_tree(type_map, arg);
                std.debug.print(", ", .{});
            }
            std.debug.print(")", .{});
        },
        .type_decl => |decl| {
            std.debug.print("type {s} = ", .{decl.ident.value});
            print_type(type_map, decl.ty);
            std.debug.print(";", .{});
        },
        .struct_cons => |val| {
            print_type(type_map, val.ty);
            std.debug.print(" {{ ", .{});
            var iter = val.fields.iterator();
            while (iter.next()) |entry| {
                std.debug.print("{s}: ", .{entry.key_ptr.*});
                print_tree(type_map, entry.value_ptr.*);
                std.debug.print(", ", .{});
            }
            std.debug.print(" }}", .{});
        },
        .enum_cons => |val| {
            print_type(type_map, val.ty);
            std.debug.print(".{s}", .{val.ident.value});
            if (val.init) |init| {
                std.debug.print("(", .{});
                print_tree(type_map, init);
                std.debug.print(")", .{});
            }
        },
        .return_stmt => |stmt| {
            std.debug.print("return ", .{});
            print_tree(type_map,stmt);
        },
        .access_operator => |exp| {
            print_tree(type_map, exp.left);
            std.debug.print(".{s}", .{exp.right.value});
        },
        else => |thing| std.debug.print("unkown: {any}\n", .{thing}),
    }
}




pub fn main() !u8 {
    const page_allocator = std.heap.page_allocator;
    var arena_alloc = std.heap.ArenaAllocator.init(page_allocator);
const gpa = arena_alloc.allocator();
    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    if (args.len < 2) {
        return error.NotEnoughCmdlineArgs;
    }

    const file = try std.fs.cwd().openFile(args[1], .{});
    defer file.close();

    source = try file.readToEndAlloc(gpa, std.math.maxInt(usize));
    var session = diag.Session.init(gpa, source);
    var type_map = try types.init_type_map(gpa); 
    var parser = parse.init_from_source(source, &session, &type_map, gpa);
    var context = try sema.init_context(source, &session, &type_map, gpa);
    const trees = parser.parse() catch |err| {
        try session.flush(std.io.getStdErr().writer());
        return err;
    };
    sema.resolve(&context, trees) catch |err| {
        try session.flush(std.io.getStdErr().writer());
        return err;
    };
    try session.flush(std.io.getStdErr().writer());

    for (trees) |tree| {
        print_tree(&type_map, tree);
        std.debug.print("\n", .{});
    }


    return 0;
}
