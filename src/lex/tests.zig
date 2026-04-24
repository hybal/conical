const std = @import("std");
const lex = @import("lexer.zig");
const common = @import("common");

const expectEqual = std.testing.expectEqual;

const allocator = std.testing.allocator_instance.allocator();

test "lex/tokens" {
    const buffer = \\
        \\ + ==    1 1. 1.0 -2 0xFF 0b1010 0o1230 let ident _ident2 _ "string" 'c' 'abc "\\n\"" 
        \\ " Δx "
        \\ //Single line comment
        \\ /* muliline comment */
        \\ < << <= <<=
        \\ mod a;
        \\ fn main() {
        \\  let a = 1;
        \\ }
    ;

    var ctx: common.Context = common.Context {
        .file_store = common.span.FileStore.init(allocator),
        .session = .init(allocator),
    };
    defer ctx.deinit();

    const fileid = try ctx.file_store.put(.{ .buffer = buffer });
    
    var lexer = try lex.Lexer.init(buffer, fileid);
    try expectEqual(.plus,           lexer.next_token().tag);
    try expectEqual(.eq2,            lexer.next_token().tag);
    try expectEqual(.int_literal,    lexer.next_token().tag);
    try expectEqual(.float_literal,  lexer.next_token().tag);
    try expectEqual(.float_literal,  lexer.next_token().tag);
    try expectEqual(.minus,          lexer.next_token().tag);
    try expectEqual(.int_literal,    lexer.next_token().tag);
    try expectEqual(.int_literal,    lexer.next_token().tag);
    try expectEqual(.int_literal,    lexer.next_token().tag);
    try expectEqual(.int_literal,    lexer.next_token().tag);
    try expectEqual(.keyword_let,    lexer.next_token().tag);
    try expectEqual(.ident,          lexer.next_token().tag);
    try expectEqual(.ident,          lexer.next_token().tag);
    try expectEqual(.underscore,     lexer.next_token().tag);
    try expectEqual(.string_literal, lexer.next_token().tag);
    try expectEqual(.char_literal,   lexer.next_token().tag);
    try expectEqual(.single_quote,   lexer.next_token().tag);
    try expectEqual(.ident,          lexer.next_token().tag);
    try expectEqual(.string_literal, lexer.next_token().tag);
    try expectEqual(.string_literal, lexer.next_token().tag); 
    try expectEqual(.lt,             lexer.next_token().tag); 
    try expectEqual(.lt2,            lexer.next_token().tag); 
    try expectEqual(.lteq,           lexer.next_token().tag); 
    try expectEqual(.lt2eq,          lexer.next_token().tag); 
    try expectEqual(.keyword_mod,    lexer.next_token().tag);
    try expectEqual(.ident,          lexer.next_token().tag);
    try expectEqual(.semicolon,      lexer.next_token().tag);
    try expectEqual(.keyword_fn,     lexer.next_token().tag);
    try expectEqual(.ident,          lexer.next_token().tag);
    try expectEqual(.open_paren,     lexer.next_token().tag);
    try expectEqual(.close_paren,    lexer.next_token().tag);
    try expectEqual(.open_bracket,   lexer.next_token().tag);
    try expectEqual(.keyword_let,    lexer.next_token().tag);
    try expectEqual(.ident,          lexer.next_token().tag);
    try expectEqual(.eq,             lexer.next_token().tag);
    try expectEqual(.int_literal,    lexer.next_token().tag);
    try expectEqual(.semicolon,      lexer.next_token().tag);
    try expectEqual(.close_bracket,  lexer.next_token().tag);

    try expectEqual(.eof,            lexer.next_token().tag);

    //TODO: add negative tests
}

fn span(a: usize, b: usize, file: common.FileId) common.Span {
    return common.Span { .start = a, .end = b, .fileid = file };
}

// Tests that the lexer tracks source spans correctly.
test "lex/spans" {
    // Starts at 1
    const buffer = \\
         \\fn main() {
         \\  let a = 1;
         \\}
    ;
    var ctx = common.Context {
        .file_store = .init(allocator),
        .session = .init(allocator),
    };
    defer ctx.deinit();


    const fileid = try ctx.file_store.put(.{ .buffer = buffer });

    var lexer = try lex.Lexer.init(buffer, fileid);

    try expectEqual(span(1, 3, fileid),   lexer.next_token().span);
    try expectEqual(span(4, 8, fileid),   lexer.next_token().span);
    try expectEqual(span(8, 9, fileid),   lexer.next_token().span);
    try expectEqual(span(9, 10, fileid),  lexer.next_token().span);
    try expectEqual(span(11, 12, fileid), lexer.next_token().span);
    try expectEqual(span(15, 18, fileid), lexer.next_token().span);
    try expectEqual(span(19, 20, fileid), lexer.next_token().span);
    try expectEqual(span(21, 22, fileid), lexer.next_token().span);
    try expectEqual(span(23, 24, fileid), lexer.next_token().span);
    try expectEqual(span(24, 25, fileid), lexer.next_token().span);
    try expectEqual(span(26, 27, fileid), lexer.next_token().span);
}
