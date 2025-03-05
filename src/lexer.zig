const std = @import("std");


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
    plus2, //++
    minus, //-
    minus2, //--
    slash, // /
    back_slash, //\
    star, //*
    caret, //^
    tilde, //~
    percent, //%
    at, //@
    dollar, //$
    semicolon,
    comma,
    question, //?
    question2, //??
    pipe, //|
    pipe2, //||
    hash, //#
    bang, // !
    bang2, // !!
    amp, //&
    amp2, //&&
    eq, //=
    eq2, //==
    noteq, // !=
    gt, //>
    lt,//<
    lteq, //<=
    gteq, //>=
    shl, //<<
    shr, //>>
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
    single_quote,
    //keywords
    keyword_if,
    keyword_else,
    keyword_while,
    keyword_for,
    keyword_in,
    keyword_match,
    keyword_fn,
    keyword_async,
    keyword_await,
    keyword_inline,
    keyword_extern,
    keyword_priv,
    keyword_pub,
    keyword_export,
    keyword_let,
    keyword_mut,
    keyword_return,
    keyword_break,
    keyword_struct,
    keyword_enum,
    keyword_test,
    keyword_union,
    keyword_use,
    keyword_mod,
    keyword_comptime,
    keyword_continue,
    keyword_true,
    keyword_false,
};

const Base = enum {
    b2,
    b8,
    b10,
    b16,
    b64
};

fn is_base(char: u8, base: Base) bool {
    return switch (base) {
        .b2 => char == '0' or char == '1',
        .b8 => char >= '0' and char <= '7',
        .b10 => char >= '0' and char <= '9',
        .b16 => char >= '0' and char <= '9'
             or char >= 'a' and char <= 'f'
             or char >= 'A' and char <= 'F',
        .b64 => char >= '0' and char <= '9'
             or char >= 'a' and char <= 'z'
             or char >= 'A' and char <= 'Z'
             or char == '=',
    };
}

fn is_id_start(char: u8) bool {
    return switch (char) {
        'a'...'z',
        'A'...'Z',
        '_' => true,
        else => false
    };
}

pub const Token = struct {
    start: usize,
    end: usize,
    tag: Tag,
};

pub fn init(buffer: []const u8) Lexer {
    return .{
        .buffer = buffer,
        .index = if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else 0,
    };
}
pub const Lexer = struct {
    buffer: []const u8,
    index: usize,

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
    pub const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "if", .keyword_if },
        .{ "else", .keyword_else },
        .{ "while", .keyword_while },
        .{ "for", .keyword_for },
        .{ "in", .keyword_in},
        .{ "match", .keyword_match },
        .{ "fn", .keyword_fn },
        .{ "async", .keyword_async },
        .{ "await", .keyword_await },
        .{ "inline", .keyword_inline },
        .{ "extern", .keyword_extern },
        .{ "export", .keyword_export },
        .{ "priv", .keyword_priv },
        .{ "pub", .keyword_pub },
        .{ "let", .keyword_let },
        .{ "mut", .keyword_mut },
        .{ "return", .keyword_return },
        .{ "break", .keyword_break},
        .{ "continue", .keyword_continue },
        .{ "struct", .keyword_struct },
        .{ "enum", .keyword_enum },
        .{ "test", .keyword_test },
        .{ "union", .keyword_union },
        .{ "use", .keyword_use },
        .{ "mod", .keyword_mod },
        .{ "comptime", .keyword_comptime },
        .{ "true", .keyword_true },
        .{ "false", .keyword_false },
    });
    fn is_double(self: *Lexer) bool {
        if (!self.has_next()) return false;
        if (self.buffer[self.index] == self.buffer[self.index + 1]) {
            self.index += 1;
            return true;
        }
        return false;
    }
    fn is_next(self: *Lexer, next_char: u8) bool {
        if (!self.has_next()) return false;
        return self.peek() orelse 0 == next_char;
    }
    fn next_if(self: *Lexer, next_char: u8) ?u8 {
        if (self.peek() orelse 0 == next_char) {
            return self.next();
        }
        return null;
    }
    fn has_next(self: *Lexer) bool {
        return self.index < self.buffer.len;
    }
    fn next(self: *Lexer) ?u8 {
        if (!self.has_next()) return null;
        self.index += 1;
        return self.buffer[self.index - 1];
    }
    fn peek(self: *Lexer) ?u8 {
        if (!self.has_next()) return null;
        return self.buffer[self.index];
    }
    fn peek2(self: *Lexer) ?u8 {
        if (self.index + 1 >= self.buffer.len) return null;
        return self.buffer[self.index + 1];
    }
    fn get(self: *Lexer) u8 {
        if (self.index == 0) return self.buffer[self.index];
        return self.buffer[self.index - 1];
    }
    fn parse_line_comment(self: *Lexer) void {
        while (self.has_next()) {
            switch (self.next() orelse 0) {
                '\n', 
                0x01...0x09, 0x0b...0x1f, 0x7f => {
                    break;
                },
                else => {}
            }
        }
    }

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
    ///Assumes that the leading \ is already consumed
    pub fn parse_escape(self: *Lexer) bool {
        var size: u8 = 0;
        switch (self.next() orelse 0) {
            '0',
            'n', 't', 'r', 'v', 'f', 'b',
            '\'', '"',
            '\\' => return true,
            'x' => size = 2,
            'u' => size = 4,
            'U' => size = 8,
            else => return false
        }
        while (size > 0): (size -= 1) {
            if (!self.has_next() or !std.ascii.isHex(self.next() orelse 0)) {
                return false;
            }
        }
        return true;
    }
    pub fn next_token(self: *Lexer) Token {
        var start = self.index;
        var tag: Tag = .eof;
        while (self.has_next()) {
            switch (self.next() orelse 0) {
                ' ', '\n', '\t', '\r', => {
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
                0 =>  tag = .eof,
                '\\' => tag = .back_slash,
                '*' => tag = .star,
                '^' => tag = .caret,
                '%' => tag = .percent,
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
                '+' => tag = if (self.next_if('+')) |_| .plus2 else .plus,
                '?' => tag = if (self.next_if('?')) |_| .question2 else .question,
                '|' => tag = if (self.next_if('|')) |_| .pipe2 else .pipe,
                '.' => tag = if (self.next_if('.')) |_| .dot2 else .dot,
                ':' => tag = if (self.next_if(':')) |_| .colon2 else .colon,
                '&' => tag = if (self.next_if('&')) |_| .amp2 else .amp,
                '=' => tag = if (self.next_if('=')) |_| .eq2 
                                else if (self.next_if('>')) |_| .fat_arrow 
                                    else .eq,
                '-' => tag = if (self.next_if('-')) |_| .minus2 
                                else if (self.next_if('>')) |_| .thin_arrow 
                                    else .minus,
                '!' => tag = if (self.next_if('=')) |_| .noteq
                                else if (self.next_if('!')) |_| .bang2
                                    else .bang,
                '>' => tag = if (self.next_if('=')) |_| .gteq
                                else if (self.next_if('>')) |_| .shr
                                    else .gt,
                '<' => tag = if (self.next_if('=')) |_| .lteq
                                else if (self.next_if('<')) |_| .shl
                                    else .lt,
                
                '\'' => { //This could probably be better
                    var can_be_lifetime = false;
                    if (self.is_next('\'')) {
                        tag = .char_literal;
                        break;
                    } else if (self.next_if('\\')) |_| {
                        if (!self.parse_escape()) {
                            tag = .invalid;
                            break;
                        }
                    } else if (is_id_start(self.get())) {
                            can_be_lifetime = true;
                    }
                    

                },
                '\"' => { //This could also probably be better
                    while (self.has_next() and !self.is_next('\"')): (_ = self.next()) {
                        if (self.next_if('\\')) |_| {
                            if (!self.parse_escape()) {
                                tag = .invalid;
                            }
                        }
                        if (!self.has_next()) {
                            tag = .invalid;
                            break;
                        }
                        switch (self.peek() orelse 0) {
                            '\n', 
                            0x01...0x09, 0x0b...0x1f, 0x7f => {
                                tag = .invalid;
                                break;
                            },
                            else => {}
                        }
                    } 
                    _ = self.next();
                    if (tag != .invalid) {
                        tag = .string_literal;
                    }

                },
                '`' => {
                    while (self.has_next() and !self.is_next('`')): (_ = self.next()) {
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
                '0'...'9' => {
                    var base: Base = .b10;
                    var is_float = false;
                    if (self.get() == '0' and self.has_next()) {
                        switch (self.peek() orelse 0) {
                            'x', 'X' => base = .b16,
                            'o', 'O' => base = .b8,
                            'b', 'B' => base = .b2,
                            else => {
                            }
                        }
                        if (base != .b10) {
                            _ = self.next();
                        }
                    } 
                    while (self.has_next()): (_ = self.next()) {
                        const next_digit = self.peek() orelse 0;
                        if (next_digit == '_') {
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
                '_',
                'a'...'z',
                'A'...'Z' => {
                    while (self.has_next()): (_ = self.next()) {
                        const next_char = self.peek() orelse 0;
                        if (!is_id_start(next_char) 
                            and next_char != '_' 
                            and !is_base(next_char, .b10)) {
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
                else => tag = .invalid 
            }
            break;
        }
        return .{
            .start = start,
            .end = self.index,
            .tag = tag
        };
    }

};

test "lexer" {
    //TODO
}
