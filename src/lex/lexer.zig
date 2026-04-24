const std = @import("std");
const common = @import("common");
const iter = common.iter;



//These are all of the supported tokens
pub const Tag = enum {
    invalid,
    eof,
    //Literals
    int_literal,
    float_literal,
    string_literal,     //"..."
    raw_string_literal, //`...`
    char_literal,       //'.'
    ident,              // [a-zA-Z_] [a-zA-Z_0-9]*
                        //Symbols
    underscore,         //_
    plus,               //+
    pluseq,             //+=
    plus2,              //++
    minus,              //-
    minuseq,            //-=
    minus2,             //--
    slash,              // /
    slasheq,            // /=
    back_slash,         //\
    star,               //*
    stareq,             //*=
    caret,              //^
    careteq,            //^=
    tilde,              //~
    percent,            //%
    percenteq,          //%=
    at,                 //@
    dollar,             //$
    semicolon,          //;
    comma,              //,
    question,           //?
    question2,          //??
    pipe,               //|
    pipearrow,          //|>
    pipeeq,             //|=
    pipe2,              //||
    pipe2eq,            //||=
    hash,               //#
    bang,               // !
    bangeq,             // !=
    bang2,              // !!
    amp,                //&
    ampeq,              //&=
    amp2,               //&&
    amp2eq,             //&&=
    eq,                 //=
    eq2,                //==
    gt,                 //>
    lt,                 //<
    lteq,               //<=
    gteq,               //>=
    lt2,                //<<
    lt2eq,              //<<=
    gt2,                //>>
    gt2eq,              //>>=
    open_bracket,       //{
    close_bracket,      //}
    open_paren,         //(
    close_paren,        //)
    open_square,        //[
    close_square,       //]
    thin_arrow,
    fat_arrow,          //=>
    dot,                //.
    dot2,               //..
    colon,              //:
    colon2,             //::
    single_quote,       //'
                        //keywords
    keyword_if,         //KEYWORD_IF
    keyword_else,       //KEYWORD_ELSE
    keyword_while,      //KEYWORD_WHILE
    keyword_for,        //KEYWORD_FOR
    keyword_loop,       //KEYWORD_LOOP
    keyword_continue,   //KEYWORD_CONTINUE
    keyword_break,      //KEYWORD_BREAK
    keyword_in,         //KEYWORD_IN
    keyword_match,      //KEYWORD_MATCH
    keyword_fn,         //KEYWORD_FN
    keyword_inline,     //KEYWORD_INLINE
    keyword_pub,        //KEYWORD_PUB
    keyword_export,     //KEYWORD_EXPORT
    keyword_extern,     //KEYWORD_EXTERN
    keyword_import,     //KEYWORD_IMPORT
    keyword_let,        //KEYWORD_LET
    keyword_mut,        //KEYWORD_MUT
    keyword_alias,      //KEYWORD_ALIAS
    keyword_move,       //KEYWORD_MOVE
    keyword_return,     //KEYWORD_RETURN
    keyword_struct,     //KEYWORD_STRUCT
    keyword_enum,       //KEYWORD_ENUM
    keyword_use,        //KEYWORD_USE
    keyword_mod,        //KEYWORD_MOD
    keyword_comptime,   //KEYWORD_COMPTIME
    keyword_as,         //KEYWORD_AS
    keyword_static,     //KEYWORD_STATIC
    keyword_type,       //KEYWORD_TYPE
    keyword_const,      //KEYWORD_CONST
    keyword_impl,       //KEYWORD_IMPL
    keyword_when,       //KEYWORD_WHEN
    keyword_Self,       //KEYWORD_SELF
    keyword_where,      //KEYWORD_WHERE
    keyword_with,       //KEYWORD_WITH
    keyword_macro,      //KEYWORD_MACRO
    keyword_pure,       //KEYWORD_PURE
    keyword_true,       //KEYWORD_TRUE
    keyword_false,      //KEYWORD_FALSE

    pub fn to_string(self: *const @This()) []const u8 {
        return switch (self.*) {
            .invalid            => "<INVALID>",
            .eof                => "<EOF>",
            .int_literal        => "integer literal",
            .float_literal      => "float literal",
            .string_literal     => "string literal",
            .raw_string_literal => "raw string literal",
            .char_literal       => "character literal",
            .ident              => "identifier",
            .underscore         => "_",
            .plus               => "+",
            .pluseq             => "+=",
            .plus2              => "++",
            .minus              => "-",
            .minuseq            => "-=",
            .minus2             => "--",
            .slash              => "/",
            .slasheq            => "/=",
            .back_slash         => "\\",
            .star               => "*",
            .stareq             => "*=",
            .caret              => "^",
            .careteq            => "^=",
            .tilde              => "~",
            .percent            => "%",
            .percenteq          => "%=",
            .at                 => "@",
            .dollar             => "$",
            .semicolon          => ";",
            .comma              => ",",
            .question           => "?",
            .question2          => "??",
            .pipe               => "|",
            .pipearrow          => "|>",
            .pipeeq             => "|=",
            .pipe2              => "||",
            .pipe2eq            => "||=",
            .hash               => "#",
            .bang               => "!",
            .bangeq             => "!=",
            .bang2              => "!!",
            .amp                => "&",
            .ampeq              => "&=",
            .amp2               => "&&",
            .amp2eq             => "&&=",
            .eq                 => "=",
            .eq2                => "==",
            .gt                 => ">",
            .lt                 => "<",
            .lteq               => "<=",
            .gteq               => ">=",
            .lt2                => "<<",
            .lt2eq              => "<<=",
            .gt2                => ">>",
            .gt2eq              => ">>=",
            .open_bracket       => "{",
            .close_bracket      => "}",
            .open_paren         => "(",
            .close_paren        => ")",
            .open_square        => "[",
            .close_square       => "]",
            .thin_arrow         => "->",
            .fat_arrow          => "=>",
            .dot                => ".",
            .dot2               => "..",
            .colon              => ":",
            .colon2             => "::",
            .single_quote       => "'",
            //keywords
            .keyword_if         => "if",
            .keyword_else       => "else",
            .keyword_while      => "while",
            .keyword_for        => "for",
            .keyword_loop       => "loop",
            .keyword_continue   => "continue",
            .keyword_break      => "break",
            .keyword_in         => "in",
            .keyword_match      => "match",
            .keyword_fn         => "fn",
            .keyword_inline     => "inline",
            .keyword_pub        => "pub",
            .keyword_export     => "export",
            .keyword_extern     => "extern",
            .keyword_import     => "import",
            .keyword_let        => "let",
            .keyword_mut        => "mut",
            .keyword_alias      => "alias",
            .keyword_move       => "move",
            .keyword_return     => "return",
            .keyword_struct     => "struct",
            .keyword_enum       => "enum",
            .keyword_use        => "use",
            .keyword_mod        => "mod",
            .keyword_comptime   => "comptime",
            .keyword_as         => "as",
            .keyword_static     => "static",
            .keyword_type       => "type",
            .keyword_const      => "const",
            .keyword_impl       => "impl",
            .keyword_when       => "when",
            .keyword_Self       => "self",
            .keyword_where      => "where",
            .keyword_with       => "with",
            .keyword_macro      => "macro",
            .keyword_pure       => "pure",
            .keyword_true       => "true",
            .keyword_false      => "false",

        };
    }


};
//All of the builtin keywords
pub const keywords = std.StaticStringMap(Tag).initComptime(.{ 
    .{ "if",       .keyword_if       },
    .{ "else",     .keyword_else     },
    .{ "while",    .keyword_while    },
    .{ "for",      .keyword_for      },
    .{ "loop",     .keyword_loop     },
    .{ "continue", .keyword_continue },
    .{ "break",    .keyword_break    },
    .{ "in",       .keyword_in       },
    .{ "match",    .keyword_match    },
    .{ "fn",       .keyword_fn       },
    .{ "inline",   .keyword_inline   },
    .{ "pub",      .keyword_pub      },
    .{ "export",   .keyword_export   },
    .{ "extern",   .keyword_extern   },
    .{ "import",   .keyword_import   },
    .{ "let",      .keyword_let      },
    .{ "mut",      .keyword_mut      },
    .{ "alias",    .keyword_alias    },
    .{ "return",   .keyword_return   },
    .{ "struct",   .keyword_struct   },
    .{ "enum",     .keyword_enum     },
    .{ "use",      .keyword_use      },
    .{ "mod",      .keyword_mod      },
    .{ "comptime", .keyword_comptime },
    .{ "as",       .keyword_as       },
    .{ "static",   .keyword_static   },
    .{ "type",     .keyword_type     },
    .{ "const",    .keyword_const    },
    .{ "impl",     .keyword_impl     },
    .{ "when",     .keyword_when     },
    .{ "Self",     .keyword_Self     },
    .{ "where",    .keyword_where    },
    .{ "with",     .keyword_with     },
    .{ "macro",    .keyword_macro    },
    .{ "pure",     .keyword_pure     },
    .{ "true",     .keyword_true     },
    .{ "false",    .keyword_false    },
});

//a token consists of a span (the location in the source code the token is located)
//and a tag (the actual type of token)
pub const Token = struct {
    span: common.Span,
    tag: Tag,

    pub fn to_string(self: *const @This(), source: []const u8) []const u8 {
        return switch (self.*.tag) {
            .ident, .int_literal, .float_literal, .raw_string_literal,
            .string_literal, .char_literal => self.span.get_string(source),
            else => self.tag.to_string(),
        };
    }
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
fn is_base(char: u21, base: Base) bool {
    return switch (base) {
        .b2 => char == '0' or char == '1',
        .b8 => char >= '0' and char <= '7',
        .b10 => char >= '0' and char <= '9',
        .b16 => char >= '0' and char <= '9' or char >= 'a' and char <= 'f' or char >= 'A' and char <= 'F',
        .b64 => char >= '0' and char <= '9' or char >= 'a' and char <= 'z' or char >= 'A' and char <= 'Z' or char == '=',
    };
}

// Check if 'char' is a valid starting character for an identifier
fn is_id_start(char: u21) bool {
    return switch (char) {
        'a'...'z', 'A'...'Z', '_' => true,
        else => false,
    };
}

fn is_hex(char: u21) bool {
    return switch (char) {
        '0'...'9', 'A'...'F', 'a'...'f' => true,
        else => false,
    };
}


pub const Lexer = struct {
    source: []const u8,
    iterator: iter.Iterator,
    index: usize, 
    _peeked: ?Token = null,
    file: common.FileId,

    pub fn init(source: []const u8, file: common.FileId) iter.UnicodeError!Lexer {
        const stripped_source = strip_bom(source);
        const self = @This() {
            .source = stripped_source,
            .iterator = try .init(stripped_source),
            .index = if (stripped_source.len < source.len) 3 else 0,
            .file = file,
        };
        return self;

    }


    // Strip any preceeding byte-order-marks (BOM)
    fn strip_bom(source: []const u8) []const u8 {
        const bytes = source[0..3];
        if (bytes[0] == 0xEF 
            and bytes[1] == 0xBB 
            and bytes[2] == 0xBF) {
            return source[3..];
        }
        return source;

    }
    //checks if the next character and the one after are the same
    fn is_double(self: *Lexer) bool {
        self.iterator.save();
        defer self.iterator.restore();
        const a = self.iterator.next();
        const b = self.iterator.next();
        if (a == null or b == null) return false;
        if (a == b) {
            return true;
        }
        return false;
    }

    //checks if the next character matches next_char
    fn is_next(self: *Lexer, next_char: u21) bool {
        if (!self.has_next()) return false;
        return self.peek() orelse 0 == next_char;
    }
    //checks if the next character matches and if so advances the lexer
    fn next_if(self: *Lexer, next_char: u21) ?u21 {
        if (self.peek() orelse 0 == next_char) {
            return self.next();
        }
        return null;
    }
    //checks if we have reached the end of the source
    //in the future it might be wise to include checks for null characters here as well
    pub fn has_next(self: *Lexer) bool {
        return self.iterator.has_next();
    }

    //gets the next character and advances
    fn next(self: *Lexer) ?u21 {
        if (!self.has_next()) return null;
        const len = self.iterator.ncLength();
        const out = self.iterator.next();
        self.index += len;
        return out;
    }
    //gets the next character but does not advance
    fn peek(self: *Lexer) ?u21 {
        if (!self.has_next()) return null;
        return self.iterator.peek();
    }
    //gets the character 2 after this one
    fn peek2(self: *Lexer) ?u21 {
        const saved_index = self.index;
        defer self.index = saved_index;
        self.iterator.save();
        defer self.iterator.restore();
        _ = self.next();

        return self.peek();
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
        var depth: usize = 1;
        while (self.has_next()) {
            const c = self.next() orelse 0;
            if (c == '/' and self.next_if('*') != null) {
                depth += 1;
            } else if (c == '*' and depth > 0 and self.next_if('/') != null) {
                depth -= 1;
                if (depth == 0) return true;
            }
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
            if (!self.has_next() or !is_hex(self.next() orelse 0)) {
                return false;
            }
        }
        return true;
    }
    //This returns the next token but does not advance
    pub fn peek_token(self: *Lexer) Token {
        const saved_index = self.index;
        self.iterator.save();
        const tok = self.next_token();
        self.iterator.restore();
        self.index = saved_index;
        return tok;
    }

    fn skip_whitespace(self: *Lexer) void {
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
        var tag: Tag = .eof;
        self.skip_whitespace();
        var start = self.index;
        while (self.has_next()) {
            self.skip_whitespace();
            start = self.index;
            const c = self.next() orelse 0;
            switch (c) {
                '/' => {
                    if (self.next_if('/')) |_| {
                        self.parse_line_comment();
                        continue;
                    } else if (self.next_if('*') != null) {
                        if (!self.parse_block_comment()) {
                            std.debug.print("DEBUG Z\n", .{});
                            tag = .invalid;
                        }
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
                '|' => tag = if (self.next_if('|')) |_| if (self.next_if('=')) |_| .pipe2eq else .pipe2 else if (self.next_if('=')) |_| .pipeeq else if (self.next_if('>')) |_| .pipearrow else .pipe,
                '.' => tag = if (self.next_if('.')) |_| .dot2 else .dot,
                ':' => tag = if (self.next_if(':')) |_| .colon2 else .colon,
                '&' => tag = if (self.next_if('&')) |_| if (self.next_if('=')) |_| .amp2eq else .amp2 else .amp,
                '=' => tag = if (self.next_if('=')) |_| .eq2 else if (self.next_if('>')) |_| .fat_arrow else .eq,
                '-' => tag = if (self.next_if('-')) |_| .minus2 else if (self.next_if('>')) |_| .thin_arrow else .minus,
                '!' => tag = if (self.next_if('=')) |_| .bangeq else if (self.next_if('!')) |_| .bang2 else .bang,
                '>' => tag = if (self.next_if('=')) |_| .gteq else if (self.next_if('>')) |_| if (self.next_if('=')) |_| .gt2eq else .gt2 else .gt,
                '<' => tag = if (self.next_if('=')) |_| .lteq else if (self.next_if('<')) |_| if (self.next_if('=')) |_| .lt2eq else .lt2 else .lt,

                '\'' => { //parses character literals
                          //TODO: add unicode support
                    if (self.next_if('\\')) |_| {
                        if (!self.parse_escape() or !self.is_next('\'')) {
                            std.debug.print("DEBUG A\n", .{});
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
                          //TODO: ensure unicode support

                    //Increment by 1 to skip "
                    var current = self.next();
                    while (self.has_next() and current != '\"') {
                        switch (current orelse 0) {
                            '\n', 0x01...0x08, 0x0b...0x1f, 0x7f => {
                                std.debug.print("DEBUG B\n", .{});
                                tag = .invalid;
                                _ = self.next();
                                break;
                            },
                            else => {},
                        }
                        if (current == '\\') {
                            if (!self.parse_escape()) {
                                std.debug.print("DEBUG C\n", .{});
                                tag = .invalid;
                            }
                        }
                        if (!self.has_next()) {
                            tag = .invalid;
                            std.debug.print("DEBUG D\n", .{});
                            break;
                        }
                        current = self.next();
                    }
                    if (tag != .invalid) {
                        tag = .string_literal;
                    }
                },
                '`' => { //parses literal string literals, which can span multiple lines but will treat escape sequences literally
                    while (self.has_next() and !self.is_next('`')) : (_ = self.next()) {
                        if (!self.has_next()) {
                            std.debug.print("DEBUG E\n", .{});
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
                    if (c == '0' and self.has_next()) {
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
                    while (self.has_next()) {
                        const next_char = self.peek() orelse 0;
                        if (!is_id_start(next_char) and next_char != '_' and !is_base(next_char, .b10)) {
                            break;
                        }
                        _ = self.next();
                    }
                    const id = self.source[start..self.index];
                    if (keywords.get(id)) |val| {
                        tag = val;
                    } else {
                        tag = .ident;
                        if (id.len == 1 and id[0] == '_') {
                            tag = .underscore;
                        } 
                    }
                },
                else => {
                    std.debug.print("DEBUG F: {}\n", .{c});
                    tag = .invalid;
                },
            }
            break;
        }
        return .{ .span = .{ .start = start, .end = self.index, .fileid = self.file}, .tag = tag };
    }
};

test "lexer" {
    //TODO
}
