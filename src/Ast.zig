const std = @import("std");
const mem = @import("mem.zig");
const types = @import("types.zig");

/// Represents a binary expression of the form: left op right
pub const BinaryExpr = struct {
    op: types.Token, //the operation
    left: *Ast, //left hand side of the operation
    right: *Ast, //right hand side of the operation
};

/// Represents a unary expression of the form: op expr
pub const UnaryExpr = struct {
    op: types.Token, //the operation
    expr: *Ast //the operation its applied to
};


/// Represents an lvalue or a value that is able to be used in an assignment operation: lvalue op= expr
pub const LValue = struct {
    ident: Ident,
    derefs: usize, // the number of dereferences being applied
    array_access: ?[]*Ast // a list of array accesses and the index being acessed
};

/// Represents an assignment operaton including compound assignment operations like += or *=
pub const Assignment = struct {
    op: types.Token,
    lvalue: LValue,
    expr: *Ast
};

/// Represents a builtin primitive type
pub const PrimitiveType = enum {
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,
    U8,
    U16,
    U32,
    U64,
    U128,
    USize,
    F32,
    F64,
    F128,
    Bool,
    Str,
    Rune, //may not be needed
    Unit,
    pub fn get_string(self: *const @This()) []const u8 {
        return switch (self.*) {
            .I8 => "i8",
            .I16 => "i16",
            .I32 => "i32",
            .I64 => "i64",
            .I128 => "i128",
            .ISize => "isize",
            .U8 => "u8",
            .U16 => "u16",
            .U32 => "u32",
            .U64 => "u64",
            .U128 => "u128",
            .USize => "usize",
            .F32 => "f32",
            .F64 => "f64",
            .F128 => "f128",
            .Bool => "bool",
            .Str => "str",
            .Rune => "rune",
            .Unit => "()",
        };
    }
    pub const prims = std.StaticStringMap(@This()).initComptime(.{
        .{ "i8", .I8 },
        .{ "i16", .I16 },
        .{ "i32", .I32 },
        .{ "i64", .I64 },
        .{ "i128", .I128 },
        .{ "isize", .ISize },
        .{ "u8", .U8 },
        .{ "u16", .U16 },
        .{ "u32", .U32 },
        .{ "u64", .U64 },
        .{ "u128", .U128 },
        .{ "usize", .USize },
        .{ "f32", .F32 },
        .{ "f64", .F64 },
        .{ "f128", .F128 },
        .{ "bool", .Bool },
        .{ "str", .Str },
        .{ "rune", .Rune },
        .{ "()", .Unit },
    });
    pub fn from_string(str: []const u8) ?@This() {
        return prims.get(str);
    }
    //will need to decide if c types are builtin or in stdlib
};

pub const TypeModifier = union(enum) {
    Array: types.Token,
    Slice,
    Ref,
    Vector,
    Comptime,
    Mut,
    Const,
    RefMut,
    RefConst,
    Ptr,
    PtrMut,
    PtrConst,
    
    pub fn get_string(self: *const TypeModifier, source: []const u8) []const u8 {
        switch (self.*) {
            .Array => |tok| return tok.span.get_string(source),
            .Slice => return "[]",
            .Ref => return "&",
            .Mut => return "mut",
            .Const => return "const",
            .RefMut => return "&mut",
            .RefConst => return "&const",
            .Ptr => return "*",
            .PtrMut => return "*mut",
            .PtrConst => return "*const",
            .Comptime => return "comp",
            else => unreachable
        }
    }
};

pub const Ident = struct {
    span: types.Span,
};
pub const Type = struct {
    base_type: union(enum) {
        primitive: PrimitiveType,
        user: Ident
    },
    modifiers: ?[]TypeModifier,
};


pub const VarDecl = struct {
    ident: Ident,
    ty: ?Type,
    is_mut: bool,
    initialize: ?*Ast
};

pub const GlobalDeclMod = enum {
    Pub,
    Extern,
    Export,
};

pub const FnModifier = union(enum) {
    Pure,
    Async,
    CompTime
};
pub const FnDecl = struct {
    decl_mod: ?GlobalDeclMod,
    fn_mod: ?FnModifier,
    ident: Ident,
    params: []Ident, //TODO: move parameter list into a list of VarDecl
    param_types: []Type,
    return_ty: Type,
    body: ?*Ast
};
pub const IfStmt = struct {
    condition: *Ast,
    block: *Ast,
    else_block: ?*Ast
};

pub const WhileLoop = struct {
    condition: *Ast,
    block: *Ast
};

pub const Ternary = struct {
    condition: *Ast,
    true_path: *Ast,
    false_path: *Ast
};

pub const Block = struct {
    exprs: []*Ast
};

pub const OptionalBlock = struct { //TODO: Merge with block
    exprs: []*Ast
};


pub const Ast = struct {
    node: AstNode,
    span: types.Span,
    pub fn create(node: AstNode, span: types.Span) @This() {
        return .{
            .node = node,
            .span = span
        };
    }
};

pub const AstNode = union(enum) { 
    binary_expr: BinaryExpr,
    unary_expr: UnaryExpr,
    terminal: types.Token,
    assignment: Assignment, 
    if_stmt: IfStmt,
    while_loop: WhileLoop,
    ternary: Ternary,
    optional_block: OptionalBlock,
    block: Block,
    var_decl: VarDecl,
    fn_decl: FnDecl,
    terminated: *Ast,
    _,
};
