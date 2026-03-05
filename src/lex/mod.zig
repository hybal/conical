const lex = @import("lexer.zig");

pub const Tag = lex.Tag;
pub const Token = lex.Token;
pub const Lexer = lex.Lexer;

test {
    _ = @import("tests.zig");
}
