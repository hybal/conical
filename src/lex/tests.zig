const std = @import("std");
const lex = @import("lexer.zig");
const common = @import("common");

const expectEqual = std.testing.expectEqual;

// Tests that the lexer correctly tokenizes the input
// Perhaps this needs to be exhaustive?
test "lex/tokens" {
    const buffer = \\ 
        \\ + ==    1 1. 1.0 -2 0xFF 0b1010 0o1230 let ident "string" 'c' 'abc "\\n\"" 
        \\ " Δx "
        \\ //Single line comment
        \\ /* muliline comment */
    ;
    
    var lexer = lex.Lexer.init(buffer);
    try expectEqual(.plus,           lexer.next_token().tag);
    try expectEqual(.eq2,            lexer.next_token().tag);
    try expectEqual(.int_literal,    lexer.next_token().tag);
    try expectEqual(.float_literal,  lexer.next_token().tag);
    try expectEqual(.float_literal,  lexer.next_token().tag);
    //FIXME: This test will currently fail as the minus sign is currently separate from the literal
    try expectEqual(.int_literal,    lexer.next_token().tag);
    try expectEqual(.int_literal,    lexer.next_token().tag);
    try expectEqual(.int_literal,    lexer.next_token().tag);
    try expectEqual(.int_literal,    lexer.next_token().tag);
    try expectEqual(.keyword_let,    lexer.next_token().tag);
    try expectEqual(.ident,          lexer.next_token().tag);
    try expectEqual(.string_literal, lexer.next_token().tag);
    try expectEqual(.char_literal,   lexer.next_token().tag);
    try expectEqual(.single_quote,   lexer.next_token().tag);
    try expectEqual(.ident,          lexer.next_token().tag);
    try expectEqual(.string_literal, lexer.next_token().tag);
    //FIXME: Unicode needs to be supported for strings
    try expectEqual(.string_literal, lexer.next_token().tag); 
    //FIXME: For some reason the comments are giving invalid?
    try expectEqual(.eof,            lexer.next_token().tag);

    //TODO: add negative tests
}

fn span(a: usize, b: usize) common.Span {
    return common.Span { .start = a, .end = b };
}

// Tests that the lexer tracks source spans correctly.
test "lex/spans" {
    // Starts at 1
    const buffer = \\
         \\fn main() {
         \\  let a = 1;
         \\}
    ;
    var lexer = lex.Lexer.init(buffer);

    try expectEqual(span(1, 3),   lexer.next_token().span);
    try expectEqual(span(4, 8),   lexer.next_token().span);
    try expectEqual(span(8, 9),   lexer.next_token().span);
    try expectEqual(span(9, 10),  lexer.next_token().span);
    try expectEqual(span(11, 12), lexer.next_token().span);
    try expectEqual(span(15, 18), lexer.next_token().span);
    try expectEqual(span(19, 20), lexer.next_token().span);
    try expectEqual(span(21, 22), lexer.next_token().span);
    try expectEqual(span(23, 24), lexer.next_token().span);
    try expectEqual(span(24, 25), lexer.next_token().span);
    //FIXME: its giving a span of 26, 26 - likely the span end is not starting at 1
    try expectEqual(span(26, 27), lexer.next_token().span);
}
