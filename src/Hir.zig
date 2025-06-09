const std = @import("std");
const Ast = @import("Ast.zig");

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
    lhs: HirNode,
    rhs: HirNode,
    op: BinaryOp,
};

const Terminal = union(enum) {

};

const UnaryExpr = struct {
    op: UnaryOp,
    expr: HirNode,
};

const HirNode = union(enum) {
    binary_expr: *BinaryExpr,
    unary_expr: *UnaryExpr,
    terminal: 
};
