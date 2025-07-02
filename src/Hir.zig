const std = @import("std");
const Ast = @import("Ast.zig");
const types = @import("types.zig");
const mem = @import("mem.zig");

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
    BinNot,
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
    integer_literal: u128,
    float_literal: f64, 
    identifier: Ident, 
    type_literal: *Ast.Type,
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
pub const Block = struct {
    body: []Hir,
};

pub const Loop = struct {
    block: Block,
};

pub const FnDecl = struct {
    id: Ident,

};

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
    binary_expr: *BinaryExpr,
    unary_expr: *UnaryExpr,
    terminal: *Terminal,
    assignment: *Assignment,
    block: *Block,
    fn_decl: *FnDecl,
    fn_call: *FnCall,
    return_stmt: *Return,
    cast: *Cast,
    loop: *Loop,
    branch: *Branch,

};
