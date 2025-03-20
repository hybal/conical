const std = @import("std");
const enums = @import("enums.zig");

pub const Ast = union(enum) { 
    expr: struct {token: enums.Token, left: ?*Ast, right: ?*Ast, terminated: ?enums.Token}, 
    assignment: struct { id: enums.Token, op: enums.Token, expr: *Ast}, //should probably move this to stmt
    if_stmt: struct { condition: *Ast, block: *Ast, else_block: ?*Ast },
    while_loop: struct { condition: *Ast, block: *Ast },
    ternary: struct { condition: *Ast, true_path: *Ast, false_path: *Ast },
    block: struct { expr: *Ast },
    _,
    pub fn create_token(tok: enums.Token, terminated: ?enums.Token) Ast {
        return .{ .expr = .{ .token = tok, .left = null, .right = null, .terminated = terminated}};
    }
};
