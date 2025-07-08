const std = @import("std");
const Ast = @import("Ast.zig");
const types = @import("types.zig");
const mem = @import("mem.zig");

pub const DefId = u64;

pub const BinaryOp = enum {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
    BinAnd,
    BinOr,
    BinXor,
    LAnd,
    LOr,
    Eq,
    NotEq,
    Gt,
    Lt,
    GtEq,
    LtEq,
    Dot,
    Cast,
};

pub const UnaryOp = enum {
    Not,
    BinNot,
    DeRef,
    Ref,
    Minus,
};

pub const BinaryExpr = struct {
    lhs: Hir,
    rhs: Hir,
    op: BinaryOp,
};

pub const Ident = struct {
    location: types.Span,
    value: []const u8,
};

pub const Terminal = union(enum) {
    string_literal: []const u8, 
    char_literal: i32,
    integer_literal: u128,
    float_literal: f64, 
    identifier: DefId, 
    type_literal: *Ast.Type,
    bool_literal: bool,
    unit,
};

pub const UnaryExpr = struct {
    op: UnaryOp,
    expr: Hir,
};

pub const Assignment = struct {
    lvalue: Hir,
    expr: Hir,
};

pub const Binding = struct {
    id: Ident,
    ty: ?Ast.TypeId,
    is_mutable: bool,
    is_pub: bool,
    is_extern: bool,
    is_export: bool,
    is_static: bool,
    expr: Hir,
};

pub const Block = struct {
    body: []Hir,
};

pub const Loop = struct {
    block: Block,
};

pub const Fn = struct {
    id: Ident,
    parameters: []struct { id: Ident, ty: Ast.TypeId },
    return_type: Ast.TypeId,
    is_public: bool,
    is_extern: bool,
    is_export: bool,
    body: Block,
};

pub const FnCall = struct {
    expr: Hir,
    arguments: []Hir
};

pub const Branch = struct {
    a_path: Block,
    b_path: Block,
    condition: InlineExpr,
};

pub const Cast = struct {
    expr: Hir,
    tyid: Ast.TypeId
};

pub const Return = struct {
    expr: Hir,
};

pub const InlineExpr = union(enum) {
    binary_expr: *BinaryExpr,
    unary_expr: *UnaryExpr,
    terminal: *Terminal,
    fn_call: *FnCall,
    cast: *Cast,
    block: *Block,
};

pub const TopLevelExpr = union(enum) {
    assignment: *Assignment,
    func: *Fn,
    return_stmt: *Return,
    loop: *Loop,
    branch: *Branch,
    binding: *Binding,
};


//This should always be trivially copiable other than the node.
pub const Hir = struct {
    node: HirNode,
    typeid: ?Ast.TypeId = null,
    escapes: bool = false,
    span: types.Span,

    pub fn create(node: HirNode, span: types.Span) @This() {
        return .{
            .node = node,
            .span = span,
        };
    }
};

//This should always be pointer sized + the tag size
pub const HirNode = union(enum) {
    inline_expr: InlineExpr,
    top_level: TopLevelExpr
};
