const std = @import("std");
const Ast = @import("Ast.zig");
const types = @import("types.zig");

const BinaryOp = enum {
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

const UnaryOp = enum {
    Not,
    BinNot,
    DeRef,
    Ref,
    Minus,
};

const BinaryExpr = struct {
    lhs: Hir,
    rhs: Hir,
    op: BinaryOp,
};

const UnaryExpr = struct {
    op: UnaryOp,
    expr: Hir,
};

const Terminal = union(enum) {
    string_literal: []const u8, 
    integer_literal: u128,
    float_literal: f64, 
    identifier: []const u8, 
    type_literal: *Ast.Type,
    unit,
};


const Assignment = struct {
    lvalue: Hir,
    expr: Hir,
};

const Block = struct {
    exprs: []Hir,
};

const Loop = Block;

const Conditional = struct {
    condition: Hir,
    then_block: Hir,
    else_block: ?Hir,
};

const Cast = struct {
    expr: Hir,
    to_type: Ast.TypeId,
};


const FnCall = struct {

};

//NOTE: This should always be pointer sized
const HirNode = union(enum) {
    binary_expr: *BinaryExpr,
    unary_expr: *UnaryExpr,
    terminal: *Terminal,
    assignment: *Assignment,
    block: *Block,
    loop: *Loop,
    conditional: *Conditional,
    cast: *Cast,
};

const Hir = struct {
    tyid: Ast.TypeId,
    span: types.Span,
    node: HirNode,
};
