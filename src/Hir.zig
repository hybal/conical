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
    string_literal: []const u8, 
    integer_literal: u128,
    float_literal: f64, 
    identifier: []const u8, 
    type_literal: *Ast.Type,
    unit,
};

const UnaryExpr = struct {
    op: UnaryOp,
    expr: HirNode,
};

const Assignment = struct {
    lvalue: HirNode,
    expr: HirNode,
};

//This should always be pointer sized
const HirNode = union(enum) {
    binary_expr: *BinaryExpr,
    unary_expr: *UnaryExpr,
    terminal: *Terminal,
    assignment: *Assignment,
    

};
