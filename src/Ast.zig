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

pub const OptionalBlock = struct {
    exprs: []*Ast
};

pub const Ast = union(enum) { 
    binary_expr: BinaryExpr,
    unary_expr: UnaryExpr,
    terminal: types.Token,
    assignment: Assignment, 
    if_stmt: IfStmt,
    while_loop: WhileLoop,
    ternary: Ternary,
    optional_block: OptionalBlock,
    block: Block,
    _,
};
