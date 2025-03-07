const std = @import("std");
const lex = @import("lexer.zig");

lexer: lex.Lexer,

pub fn init_from_lexer(in: lex.Lexer) @This() {
    return .{
        .lexer = in 
    };
}
pub fn init_from_source(src: []const u8) @This() {
    return .{
        .lexer = lex.init(src)
    };
}

pub fn parse(self: *@This()) void {
    _ = self;
}




