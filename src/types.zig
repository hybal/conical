const std = @import("std");
const Ast = @import("Ast.zig");

pub const Span = struct {
    start: usize,
    end: usize,
    pub fn get_string(self: *const Span, source: []const u8) []const u8 {
        return source[self.start..self.end];
    }
    pub fn merge(self: *@This(), other: Span) void {
        self.start = @min(self.start, other.start);
        self.end = @max(self.end, other.end);
    }

};

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
    tildeeq, //~=
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
    keyword_comp,
    keyword_continue,
    keyword_as,
    keyword_static,
    keyword_type,
    keyword_const,
    keyword_unsafe,
    keyword_impl,
    keyword_move,
    keyword_self,
    keyword_trait,
    keyword_when,
    keyword_Self,
    keyword_where,
    keyword_macro,
    keyword_new,
    keyword_do,
    keyword_try,
    keyword_pure,
    keyword_true,
    keyword_false,
};
pub const Token = struct {
    span: Span,
    tag: Tag,
    
};


pub const Symbol = struct {
    ty: ?Ast.TypeId,
    ident: Span,
    ast: *Ast.Ast
};


pub const SymTab = std.StringHashMap(Symbol);
pub const TypeTbl = std.HashMap(Ast.TypeId, Ast.Type, std.hash_map.AutoContext(Ast.TypeId), 80);

pub fn init_type_map(gpa: std.mem.Allocator) !TypeTbl {
    var out: TypeTbl = .init(gpa);
    inline for (@typeInfo(Ast.PrimitiveType).@"enum".fields) |field| {
        const val = Ast.Type.createPrimitive(@enumFromInt(field.value), null);
        try out.put(val.hash(), val);
    }
    return out;
}

pub const CompUnit = struct {
    file: []const u8,
    ast: []Ast.Ast,
    symbol_table: SymTab,
    type_table: TypeTbl, 
};


