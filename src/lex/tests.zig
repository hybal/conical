const std = @import("std");
const lex = @import("lexer.zig");

const expectEqual = std.testing.expectEqual;


test "lex" {
    const buffer = "+ ==    1 1. 1.0 -2 0xFF 0b1010 0o1230 let ident \"string\" 'c' \"\\n\"";
    var lexer = lex.Lexer.init(buffer);
    try expectEqual(.plus,           lexer.next_token().tag);
    try expectEqual(.eq2,            lexer.next_token().tag);
    try expectEqual(.int_literal,    lexer.next_token().tag);
    try expectEqual(.float_literal,  lexer.next_token().tag);
    try expectEqual(.float_literal,  lexer.next_token().tag);
    try expectEqual(.minus,          lexer.next_token().tag); // FIXME: This should not be a seperate token
    try expectEqual(.int_literal,    lexer.next_token().tag);
    try expectEqual(.int_literal,    lexer.next_token().tag);
    try expectEqual(.int_literal,    lexer.next_token().tag);
    try expectEqual(.int_literal,    lexer.next_token().tag);
    try expectEqual(.keyword_let,    lexer.next_token().tag);
    try expectEqual(.ident,          lexer.next_token().tag);
    try expectEqual(.string_literal, lexer.next_token().tag);
    try expectEqual(.char_literal,   lexer.next_token().tag);
    try expectEqual(.string_literal, lexer.next_token().tag);
    try expectEqual(.eof,            lexer.next_token().tag);
}
