const std = @import("std");
const lex = @import("lexer.zig");
const parse = @import("parser.zig");
pub fn main() !void {
    const source = 
        \\ let a = 1;
    ;
    var parser = parse.init_from_source(source);
    parser.parse();
    _ = lex;
}


