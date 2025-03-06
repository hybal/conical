const std = @import("std");
const lex = @import("lexer.zig");
pub fn main() !void {
    const source = 
        \\ #!/bin/bash
        \\ fn add(a,b): (i32, i32) -> i32 {
        \\     a + b
        \\ }
    ;
    var lexer = lex.init(source);
    var next = lexer.next_token();
    while (next.tag != .eof) : (next = lexer.next_token()) {
        std.debug.print("token: {}, str: \"{s}\"\n", .{next, source[next.start..next.end]});
    }
}
