const std = @import("std");
const common = @import("common");



//These are all of the supported tokens
pub const Tag = enum {
    invalid,
    eof,
    //Literals
    int_literal,
    float_literal,
    string_literal, //"..."
    raw_string_literal, //`...`
    char_literal, //'.'
    ident, // [a-zA-Z_] [a-zA-Z_0-9]*
    //Symbols
    plus, //+
    pluseq, //+=
    plus2, //++
    minus, //-
    minuseq, //-=
    minus2, //--
    slash, // /
    slasheq, // /=
    back_slash, //\
    star, //*
    stareq, //*=
    caret, //^
    careteq, //^=
    tilde, //~
    percent, //%
    percenteq, //%=
    at, //@
    dollar, //$
    semicolon, //;
    comma, //,
    question, //?
    question2, //??
    pipe, //|
    pipeeq, //|=
    pipe2, //||
    hash, //#
    bang, // !
    bang2, // !!
    amp, //&
    ampeq, //&=
    amp2, //&&
    eq, //=
    eq2, //==
    noteq, // !=
    gt, //>
    lt, //<
    lteq, //<=
    gteq, //>=
    shl, //<<
    shleq, //<<=
    shr, //>>
    shreq, //>>=
    open_bracket, //{
    close_bracket, //}
    open_paren, //(
    close_paren, //)
    open_square, //[
    close_square, //]
    thin_arrow,
    fat_arrow, //=>
    dot, //.
    dot2, //..
    colon, //:
    colon2, //::
    single_quote, //'
    //keywords
    keyword_if, //if
    keyword_else, //else 
    keyword_while, //while 
    keyword_for, //for
    keyword_in, //in
    keyword_match, //match 
    keyword_fn, //fn
    keyword_async, //async (may be removed)
    keyword_await, //await (may be removed)
    keyword_inline, //inline 
    keyword_extern, //extern
    keyword_priv, //priv (may be removed)
    keyword_pub, //pub
    keyword_export, //export
    keyword_import, //import
    keyword_let, //let
    keyword_mut, //mut
    keyword_return, //return
    keyword_break, //break
    keyword_struct, //struct
    keyword_enum, //enum
    keyword_test, //test (may be removed)
    keyword_use, //use
    keyword_mod, //mod
    keyword_comp, //comp (may be changed to 'comptime')
    keyword_continue, //continue
    keyword_as, //as
    keyword_static, //static (may be removed)
    keyword_type,  //typ
    keyword_const, //const
    keyword_unsafe, //unsafe
    keyword_impl, //impl
    keyword_when, //when (may be removed)
    keyword_Self, //Self
    keyword_where, //where
    keyword_macro, //macro
    keyword_do, //do
    keyword_pure, //pure
    keyword_yield,
    keyword_true, //true
    keyword_false, //false
};

//a token consists of a span (the location in the source code the token is located)
//and a tag (the actual type of token)
pub const Token = struct {
    span: common.Span,
    tag: Tag,
};


const Base = enum { 
    b2, 
    b8, 
    b10, 
    b16, 
    b64 
};

//Returns whether or not 'char' is a certain base (one of b2, b8, b10, b16, or b64)
//this is used when parsing integer literals
fn is_base(char: u8, base: Base) bool {
    return switch (base) {
        .b2 => char == '0' or char == '1',
        .b8 => char >= '0' and char <= '7',
        .b10 => char >= '0' and char <= '9',
        .b16 => char >= '0' and char <= '9' or char >= 'a' and char <= 'f' or char >= 'A' and char <= 'F',
        .b64 => char >= '0' and char <= '9' or char >= 'a' and char <= 'z' or char >= 'A' and char <= 'Z' or char == '=',
    };
}

//This function is what determines what characters can come at the beginning of an identifier
fn is_id_start(char: u8) bool {
    return switch (char) {
        'a'...'z', 'A'...'Z', '_' => true,
        else => false,
    };
}

//The overall lexer struct
pub const Lexer = struct {
    buffer: []const u8, //the provided source code

    //the current index
    //note that it actually stores the next character to process
    //so the current character is actually index - 1
    index: usize, 

    pub fn init(buffer: []const u8) Lexer {
        return .{
            .buffer = buffer,
            .index = 0,
            // I dont remember why I did this so I removed it for now.
            //.index = if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else 0,
        };
    }

    //The current state of the lexer, it is mostly used for parsing variable-width tokens such as literals and comments
    const State = enum { 
        start,
        end,
        escape_char,
        string_literal,
        raw_string_literal,
        char_literal,
        number_literal,
        line_comment,
        block_comment, 
        ident 
    };

    //All of the builtin keywords
    pub const keywords = std.StaticStringMap(Tag).initComptime(.{ 
        .{ "if", .keyword_if }, 
        .{ "else", .keyword_else },
        .{ "while", .keyword_while },
        .{ "for", .keyword_for },
        .{ "in", .keyword_in },
        .{ "match", .keyword_match },
        .{ "fn", .keyword_fn },
        .{ "async", .keyword_async },
        .{ "await", .keyword_await },
        .{ "inline", .keyword_inline },
        .{ "extern", .keyword_extern },
        .{ "export", .keyword_export }, 
        .{ "import", .keyword_import },
        .{ "priv", .keyword_priv },
        .{ "pub", .keyword_pub },
        .{ "let", .keyword_let },
        .{ "mut", .keyword_mut },
        .{ "return", .keyword_return },
        .{ "break", .keyword_break },
        .{ "continue", .keyword_continue },
        .{ "struct", .keyword_struct },
        .{ "enum", .keyword_enum },
        .{ "test", .keyword_test }, 
        .{ "use", .keyword_use },
        .{ "mod", .keyword_mod },
        .{ "comp", .keyword_comp },
        .{ "true", .keyword_true },
        .{ "false", .keyword_false },
        .{ "as", .keyword_as },
        .{ "static", .keyword_static },
        .{ "type", .keyword_type },
        .{ "const", .keyword_const },
        .{ "unsafe", .keyword_unsafe },
        .{ "impl", .keyword_impl },
        .{ "when", .keyword_when },
        .{ "Self", .keyword_Self },
        .{ "where", .keyword_where }, 
        .{ "macro", .keyword_macro },
        .{ "do", .keyword_do }, 
        .{ "pure", .keyword_pure },
        .{ "yield", .keyword_yield },
    });

    //checks if the next character and the one after are the same
    fn is_double(self: *Lexer) bool {
        if (!self.has_next()) return false;
        if (self.buffer[self.index] == self.buffer[self.index + 1]) {
            self.index += 1;
            return true;
        }
        return false;
    }

    //checks if the next character matches next_char
    fn is_next(self: *Lexer, next_char: u8) bool {
        if (!self.has_next()) return false;
        return self.peek() orelse 0 == next_char;
    }
    //checks if the next character matches and if so advances the lexer
    fn next_if(self: *Lexer, next_char: u8) ?u8 {
        if (self.peek() orelse 0 == next_char) {
            return self.next();
        }
        return null;
    }
    //checks if we have reached the end of the source
    //in the future it might be wise to include checks for null characters here as well
    pub fn has_next(self: *Lexer) bool {
        return self.index < self.buffer.len - 1;
    }

    //gets the next character and advances
    fn next(self: *Lexer) ?u8 {
        if (!self.has_next()) return null;
        self.index += 1;
        return self.buffer[self.index - 1];
    }
    //gets the next character but does not advance
    fn peek(self: *Lexer) ?u8 {
        if (!self.has_next()) return null;
        return self.buffer[self.index];
    }
    //gets the character 2 after this one
    fn peek2(self: *Lexer) ?u8 {
        if (self.index + 1 >= self.buffer.len) return null;
        return self.buffer[self.index + 1];
    }
    //gets the current character (index - 1)
    pub fn get(self: *Lexer) u8 {
        if (self.index == 0) return self.buffer[self.index];
        return self.buffer[self.index - 1];
    }

    //parses a line comment (one that starts with //)
    //line comments go until it either encounters a newline character or 
    //reaches the end of the file
    fn parse_line_comment(self: *Lexer) void {
        while (self.has_next()) {
            switch (self.peek() orelse 0) {
                '\n', 0x01...0x09, 0x0b...0x1f, 0x7f, 0 => {
                    break;
                },
                else => _ = self.next(),
            }
        }
    }

    //parses a block comment
    //block comments start with /* and end with */ and can be nested
    fn parse_block_comment(self: *Lexer) bool {
        var depth: usize = 0;
        while (self.has_next()) {
            if (self.get() == '/' and self.next_if('*') != null) {
                depth += 1;
            } else if (self.get() == '*' and depth > 0 and self.next_if('/') != null) {
                depth -= 1;
                if (depth == 0) return true;
            }
            _ = self.next();
        }
        return false;
    }

    //this parses the supported escape codes in a character or string literal
    //these start with a backslash (\) and are used to insert non-printable characters
    //as well as arbitrary character codes using hex.
    //note: this assumes that the leading \ is already consumed
    pub fn parse_escape(self: *Lexer) bool {
        var size: u8 = 0;
        switch (self.next() orelse 0) {
            '0', 'n', 't', 'r', 'v', 'f', 'b', '\'', '"', '\\' => return true,
            'x' => size = 2,
            'u' => size = 4,
            'U' => size = 8,
            else => return false,
        }
        while (size > 0) : (size -= 1) {
            if (!self.has_next() or !std.ascii.isHex(self.next() orelse 0)) {
                return false;
            }
        }
        return true;
    }
    //this is meant to be used outside of the lexer
    //it checks if one of the array of Tags matches the next token 
    //and if so it will consume and return it.
    //if not it will backtrack and return null
    pub fn consume_if_eq(self: *Lexer, one_of: []const Tag) ?Token {
        const save = self.index;
        const next_tok = self.next_token();
        for (one_of) |tok| {
            if (next_tok.tag == tok) {
                return next_tok;
            }
        }
        self.index = save;
        return null;
    }

    //This just checks if the next token matches 'tag', it does not consume it
    pub fn is_next_token(self: *Lexer, tag: Tag) bool {
        return self.peek_token().tag == tag;
    }
    //This returns the next token but does not advance
    pub fn peek_token(self: *Lexer) Token {
        const saved = self.index;
        const out = self.next_token();
        self.index = saved;
        return out;
    }

    pub fn skip_whitespace(self: *Lexer) void {
        while (self.has_next()) {
            switch (self.peek() orelse 0) {
                '\n', ' ', 0x01...0x09, 0x0b...0x1f, 0x7f => {
                    _ = self.next();
                }, 
                else => break
            }
        }
    }

    //This is the function used to mainpulate the lexer
    //it will consume and return the next token in the buffer.
    //This lexer is "lazy" meaning it does not tokenize the whole source
    //but instead relies on whatever is using it to drive it.
    pub fn next_token(self: *Lexer) Token {
        var start = self.index;
        var tag: Tag = .eof;
        while (self.has_next()) {
            switch (self.next() orelse 0) {
                '\n', ' ', 0x01...0x09, 0x0b...0x1f, 0x7f => { //skip whitespace
                    start = self.index;
                    continue;
                },
                '/' => {
                    if (self.next_if('/')) |_| {
                        self.parse_line_comment();
                        continue;
                    } else if (self.is_next('*')) {
                        if (!self.parse_block_comment()) tag = .invalid;
                        continue;
                    } else {
                        tag = .slash;
                    }
                },
                0 => tag = .eof,
                '\\' => tag = .back_slash,
                '*' => tag = if (self.next_if('=')) |_| .stareq else .star, //inline ifs are used to parse fixed length multi character tokens
                '^' => tag = if (self.next_if('=')) |_| .careteq else .caret,
                '%' => tag = if (self.next_if('=')) |_| .percenteq else .percent,
                '$' => tag = .dollar,
                '(' => tag = .open_paren,
                ')' => tag = .close_paren,
                '[' => tag = .open_square,
                ']' => tag = .close_square,
                '{' => tag = .open_bracket,
                '}' => tag = .close_bracket,
                '@' => tag = .at,
                '~' => tag = .tilde,
                ';' => tag = .semicolon,
                ',' => tag = .comma,
                '#' => {
                    if (self.next_if('!')) |_| {
                        self.parse_line_comment();
                        continue;
                    }
                    tag = .hash;
                },
                '+' => tag = if (self.next_if('+')) |_| .plus2 else if (self.next_if('=')) |_| .pluseq else .plus,
                '?' => tag = if (self.next_if('?')) |_| .question2 else .question,
                '|' => tag = if (self.next_if('|')) |_| .pipe2 else if (self.next_if('=')) |_| .pipeeq else .pipe,
                '.' => tag = if (self.next_if('.')) |_| .dot2 else .dot,
                ':' => tag = if (self.next_if(':')) |_| .colon2 else .colon,
                '&' => tag = if (self.next_if('&')) |_| .amp2 else if (self.next_if('=')) |_| .ampeq else .amp,
                '=' => tag = if (self.next_if('=')) |_| .eq2 else if (self.next_if('>')) |_| .fat_arrow else .eq,
                '-' => tag = if (self.next_if('-')) |_| .minus2 else if (self.next_if('>')) |_| .thin_arrow else .minus,
                '!' => tag = if (self.next_if('=')) |_| .noteq else if (self.next_if('!')) |_| .bang2 else .bang,
                '>' => tag = if (self.next_if('=')) |_| .gteq else if (self.next_if('>')) |_| if (self.next_if('=')) |_| .shreq else .shr else .gt,
                '<' => tag = if (self.next_if('=')) |_| .lteq else if (self.next_if('<')) |_| if (self.next_if('=')) |_| .shleq else .shl else .lt,

                '\'' => { //parses character literals
                    if (self.next_if('\\')) |_| {
                        if (!self.parse_escape() or !self.is_next('\'')) {
                            tag = .invalid;
                            break;
                        }
                        _ = self.next();
                        tag = .char_literal;
                        break;
                    }
                    if (self.next_if('\'')) |_| {
                        tag = .char_literal;
                        break;
                    }
                    if (self.peek2() != '\'') {
                        tag = .single_quote;
                        break;
                    }
                    _ = self.next();
                    _ = self.next();
                    tag = .char_literal;
                },
                '\"' => { //parses string literals
                    var current = self.next();
                    while (self.has_next() and current != '\"') : (current = self.next()) {
                        switch (self.peek() orelse 0) {
                            '\n', 0x01...0x08, 0x0b...0x1f, 0x7f => {
                                tag = .invalid;
                                _ = self.next();
                                break;
                            },
                            else => {},
                        }
                        if (self.next_if('\\')) |_| {
                            if (!self.parse_escape()) {
                                tag = .invalid;
                            }
                        }
                        if (!self.has_next()) {
                            tag = .invalid;
                            break;
                        }
                    }
                    if (tag != .invalid) {
                        tag = .string_literal;
                    }
                },
                '`' => { //parses literal string literals, which can span multiple lines but will treat escape sequences literally
                    while (self.has_next() and !self.is_next('`')) : (_ = self.next()) {
                        if (!self.has_next()) {
                            tag = .invalid;
                            break;
                        }
                    }
                    _ = self.next();
                    if (tag != .invalid) {
                        tag = .raw_string_literal;
                    }
                },
                '0'...'9' => { //this parses a number literal 
                    var base: Base = .b10;
                    var is_float = false;
                    if (self.get() == '0' and self.has_next()) {
                        switch (self.peek() orelse 0) {
                            'x', 'X' => base = .b16, //the number can be prefixed with a 0 and a character that indicates the base
                            'o', 'O' => base = .b8,
                            'b', 'B' => base = .b2,
                            else => {}, //currently base 64 is not supported since I have not figured out a good syntax for it
                        }
                        if (base != .b10) {
                            _ = self.next();
                        }
                    }
                    while (self.has_next()) : (_ = self.next()) {
                        const next_digit = self.peek() orelse 0;
                        if (next_digit == '_') { //numbers can contain underscores (_) as optional seperators
                            continue;
                        }
                        if (next_digit == '.') {
                            if (self.peek2() orelse 0 == '.') {
                                break;
                            }
                            if (self.peek2()) |val| {
                                if (is_id_start(val)) {
                                    break;
                                }
                            }
                            if (is_float) {
                                break;
                            }
                            is_float = true;
                            continue;
                        }
                        if (!is_base(next_digit, base)) {
                            break;
                        }
                    }

                    if (tag != .invalid) {
                        tag = if (is_float) .float_literal else .int_literal;
                    }
                },
                '_', 'a'...'z', 'A'...'Z' => {
                    while (self.has_next()) : (_ = self.next()) {
                        const next_char = self.peek() orelse 0;
                        if (!is_id_start(next_char) and next_char != '_' and !is_base(next_char, .b10)) {
                            break;
                        }
                    }
                    const id = self.buffer[start..self.index];
                    if (keywords.get(id)) |val| {
                        tag = val;
                    } else {
                        tag = .ident;
                    }
                },
                else => tag = .invalid,
            }
            break;
        }
        return .{ .span = .{ .start = start, .end = self.index }, .tag = tag };
    }
};

test "lexer" {
    //TODO
}
