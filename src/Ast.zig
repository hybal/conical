const std = @import("std");
const lexer = @import("lexer.zig");
pub const Ast = struct {
    token: lexer.Token,
    left: ?*Ast,
    right: ?*Ast,

    pub fn init(tok: lexer.Token) Ast {
        return .{
            .token = tok,
            .left = null,
            .right = null
        };
    }
};
