const std = @import("std");
const Ast = @import("Ast.zig");
const Hir = @import("Hir.zig");
const diag = @import("diag.zig");

pub const DefId = u64;

pub const Symbol = struct {
    //name: []const u8,
    name: Ast.Ident,
    tyid: ?Ast.TypeId,
    node: *Ast.Ast,
    qualifier: enum {
        Public,
        Private,
        Protected,
        Extern,
        Export,
    } = .Private,
    scope: enum {
        Local,
        LocalEscapes,
        Global,
        Static,
    } = .Local,
};

pub const SymbolTable = struct {
    symbol_map: std.HashMap(DefId, Symbol, std.hash_map.AutoContext(DefId), 80),
    parent: ?usize,
    children: ?[]usize,
    is_function_scope: bool = false,
};

//This represents a region in the source code
pub const Span = struct {
    start: usize,
    end: usize,
    //provided the source code, will get the actual string that this span represents
    pub fn get_string(self: *const Span, source: []const u8) []const u8 {
        return source[self.start..self.end];
    }
    //combines two spans
    pub fn merge(self: *@This(), other: Span) void {
        self.start = @min(self.start, other.start);
        self.end = @max(self.end, other.end);
    }

};

pub const ModuleId = u64;

pub const Module = struct {
    exports: std.HashMap(DefId, Symbol, std.hash_map.AutoContext(DefId), 80),
    path: Ast.Path,
    source_file: []const u8,
    imports: std.ArrayList(ModuleId),
    //TODO: add llvm or paths to object files once full compliation is done
    pub fn get_symbol(self: *@This(), defid: DefId) ?Symbol {
        if (self.symbols.get(defid)) |val| {
            return val;
        }
        return null;
    }

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(std.mem.asBytes(&self.path.hash()));
        return hasher.final();
    }
};

pub const ModuleStore = struct {
    store: std.AutoHashMap(ModuleId, Module),
    lock: std.Thread.RwLock.Impl,

    pub fn get(self: *@This(), modid: ModuleId) ?Module {
        self.lock.lockShared();
        const out = self.store.get(modid);
        self.lock.unlockShared();
        return out;
    }

    pub fn put(self: *@This(), modid: ModuleId, mod: Module) !void {
        self.lock.lock();
        try self.store.put(modid, mod);
        self.lock.unlock();
    }
};

pub const Context = struct {
    sym_tab: std.ArrayList(SymbolTable),
    type_tab: TypeTbl,
    source: []const u8,
    file_path: []const u8,
    session: diag.Session,
    module: ?ModuleId,
    module_store: *ModuleStore,
    //NOTE: eventually compiler flags will go here
};

pub const Path = struct {
    module: ModuleId,
    base: Ast.Ident,

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(std.mem.asBytes(&self.module));
        hasher.update(std.mem.asBytes(&self.base));
        return hasher.final();
    }
};




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
    span: Span,
    tag: Tag,
};


pub const TypeTbl = std.HashMap(Ast.TypeId, Ast.Type, std.hash_map.AutoContext(Ast.TypeId), 80);

pub fn init_type_map(gpa: std.mem.Allocator) !TypeTbl {
    var out: TypeTbl = .init(gpa);
    inline for (@typeInfo(Ast.PrimitiveType).@"enum".fields) |field| {
        const val = Ast.Type.createPrimitive(@enumFromInt(field.value), null);
        try out.put(val.hash(), val);
    }
    return out;
}

//currently unused (but should be)
//represents a compilation unit/module, including all of the relevant information from every stage
pub const CompUnit = struct {
    file: []const u8, //the relative path of the file 
    out_file: []const u8, //the filename of the output file
    source: []const u8, //the file source
    hir: []Hir.Hir, //the ast
    hir_info: *Hir.HirInfoTable,
    symbol_table: []SymbolTable, //the symbol table 
    type_table: *TypeTbl,  //the type table (maps typeids to types)
};


