const std = @import("std");
const types = @import("types.zig");

pub const BinaryExpr = struct {
    op: types.Token,
    left: *Ast,
    right: *Ast,
};

pub const UnaryExpr = struct {
    op: types.Token,
    expr: *Ast
};

pub const Assignment = struct {
    id: types.Token,
    op: types.Token,
    expr: *Ast
};

pub const IfStmt = struct {
    condition: *Ast,
    block: *Block,
    else_block: ?*Block
};

pub const WhileLoop = struct {
    condition: *Ast,
    block: *Block
};

pub const Ternary = struct {
    condition: *Ast,
    true_path: *Ast,
    false_path: *Ast
};

//This is a seperate structure to allow for future expansion
pub const Block = struct {
    exprs: []Ast
};

pub const Ast = union(enum) { 
    binary_expr: BinaryExpr,
    unary_expr: UnaryExpr,
    terminal: types.Token,
    assignment: Assignment, 
    if_stmt: IfStmt,
    while_loop: WhileLoop,
    ternary: Ternary,
    block: Block,
    _,
};
