const std = @import("std");
const lex = @import("lexer.zig");
const enums = @import("enums.zig");
const Ast = @import("Ast.zig").Ast;
const mem = @import("mem.zig");
gpa: std.mem.Allocator,
lexer: lex.Lexer,

pub fn init_from_lexer(in: lex.Lexer, gpa: std.mem.Allocator) @This() {
    return .{
        .lexer = in,
        .gpa = gpa
    };
}
pub fn init_from_source(src: []const u8, gpa: std.mem.Allocator) @This() {
    return .{
        .lexer = lex.Lexer.init(src),
        .gpa = gpa
    };
}

pub fn parse(self: *@This()) !*Ast {
    var ast: *Ast = undefined;
    while (self.lexer.has_next()) {
        ast = try self.assignment();
    }
    return ast;
}


fn assignment(self: *@This()) !*Ast {
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.ident})) |id| {
        if (self.lexer.consume_if_eq(&[_]enums.Tag{
        .eq, .pluseq, .minuseq, .stareq, .slasheq, .percenteq, .shleq, .shreq, .ampeq, .careteq, .pipeeq
        })) |token| {
            const parent: Ast = .{
                .token = token,
                .left = try mem.createWith(self.gpa, Ast.init(id)),
                .right = try self.expr(),
            };
            return try mem.createWith(self.gpa, parent);
        }
    }
    return self.expr();
}

fn expr(self: *@This()) !*Ast {
    var left = try self.term();
    while (self.lexer.consume_if_eq(&[_]enums.Tag{.plus, .minus})) |token| {
        const right = try self.expr();
        const parent: Ast = .{
            .token = token,
            .left = left,
            .right = right
        };
        left = try mem.createWith(self.gpa, parent);
    } 
    return left;
}
fn term(self: *@This()) !*Ast {
    var left = try self.factor();
    while (self.lexer.consume_if_eq(&[_]enums.Tag{.star, .slash, .percent})) |token| {
        const right = try self.term();
        const parent: Ast = .{
            .token = token,
            .left = left,
            .right = right
        };
        left = try mem.createWith(self.gpa, parent);
    } 
    return left;
}

fn factor(self: *@This()) anyerror!*Ast {
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.open_paren})) |_| {
        const left = try self.expr();
        if (self.lexer.consume_if_eq(&[_]enums.Tag{.close_paren}) == null) {
            return error.UnclosedParen;
        }
        return left;
    }
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.int_literal, .float_literal, .ident, .string_literal, .raw_string_literal, .char_literal})) |token| {

        return mem.createWith(self.gpa, Ast.init(token));
    } 
    return error.Invalid;
}




