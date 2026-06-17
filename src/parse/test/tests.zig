const std = @import("std");
const testing = std.testing;
const lex = @import("lex");
const common = @import("common");
const parse = @import("../parser.zig");
const golden = @import("golden.zig");
//const utils = @import("utils.zig");

test "parser" {
    const code = \\ mod a;
                 \\ fn main() {
                 \\     let a = 1;
                 \\ }
    ;

    var ctx = common.Context.init(std.heap.page_allocator);
    const file = try ctx.file_store.put(.{ .buffer = code });
    var parser = try parse.init(&ctx, code, file, std.heap.page_allocator);
    _ = try parser.parse();
    //std.debug.print("{s} \n", .{try utils.print_tree(&ctx, code, &ast, std.heap.page_allocator)});
    //defer ast.deinit(std.heap.page_allocator);
    //try format.ast_to_json(&ast);
}
