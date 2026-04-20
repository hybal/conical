const std = @import("std");
const testing = std.testing;
const lex = @import("lex");
const common = @import("common");
const parse = @import("../parser.zig");
const golden = @import("golden.zig");
const format = @import("../serialize_json.zig");

test "parser" {
    const code = \\ mod a;
                 \\ fn main() {
                 \\     let a = 1;
                 \\ }
    ;

    var ctx = common.Context.init(std.heap.page_allocator);
    const file = try ctx.file_store.put(.{ .buffer = code });
    var reader = std.Io.Reader.fixed(code);
    var parser = parse.init(&ctx, &reader, file, std.heap.page_allocator);
    const ast = try parser.parse();
    _ = ast;
    //defer ast.deinit(std.heap.page_allocator);
    //try format.ast_to_json(&ast);
}
