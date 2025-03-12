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



fn expression(self: *@This()) !*Ast {
    return try self.assignment();
}

fn assignment(self: *@This()) anyerror!*Ast {
    const saved = self.lexer.index;
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.ident})) |id| {
        if (self.lexer.consume_if_eq(&[_]enums.Tag{
            .eq, .pluseq, .minuseq, .stareq, .slasheq, .percenteq, .shleq, .shreq, .ampeq, .careteq, .pipeeq
        })) |token| {
            const parent: Ast = .{ .assignment = .{
                .id = id,
                .op = token,
                .expr = try self.ternary(),
            }};
            return try mem.createWith(self.gpa, parent);
        } else {
            self.lexer.index = saved;
        }
    }
    return self.ternary();
}

fn ternary(self: *@This()) !*Ast {
    var condition = try self.logical_or();
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.question})) |_| {
        const true_path = try self.expression();
        _ = try self.lexer.expect_token(.colon);
        const false_path = try self.ternary();
        const parent: Ast = .{ .ternary = .{
            .condition = condition,
            .true_path = true_path,
            .false_path = false_path,
        }};
        condition = try mem.createWith(self.gpa, parent);
    }
    return condition;
}

fn logical_or(self: *@This()) !*Ast {
    var left = try self.logical_and();
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.pipe2})) |pipe2| {
        const parent: Ast = .{ .expr = .{
            .token = pipe2,
            .left = left,
            .right = try self.logical_or(),
            .terminated = self.lexer.consume_if_eq(&[_]enums.Tag{.semicolon}) 
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn logical_and(self: *@This()) !*Ast {
    var left = try self.bitwise_or();
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.amp2})) |amp2| {
        const parent: Ast = .{ .expr = .{
            .token = amp2,
            .left = left,
            .right = try self.logical_and(),
            .terminated = self.lexer.consume_if_eq(&[_]enums.Tag{.semicolon}) 
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}

fn bitwise_or(self: *@This()) !*Ast {
    var left = try self.bitwise_xor();
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.pipe})) |pipe| {
        const parent: Ast = .{ .expr = .{
            .token = pipe,
            .left = left,
            .right = try self.bitwise_or(),
            .terminated = self.lexer.consume_if_eq(&[_]enums.Tag{.semicolon}) 
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn bitwise_xor(self: *@This()) anyerror!*Ast {
    var left = try self.bitwise_and();
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.caret})) |caret| {
        const parent: Ast = .{ .expr = .{
            .token = caret,
            .left = left,
            .right = try self.bitwise_xor(),
            .terminated = self.lexer.consume_if_eq(&[_]enums.Tag{.semicolon}) 
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn bitwise_and(self: *@This()) anyerror!*Ast {
    var left = try self.equality();
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.amp})) |amp| {
        const parent: Ast = .{ .expr = .{
            .token = amp,
            .left = left,
            .right = try self.bitwise_and(),
            .terminated = self.lexer.consume_if_eq(&[_]enums.Tag{.semicolon}) 
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}

fn equality(self: *@This()) anyerror!*Ast {
    var left = try self.relational();
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.eq2, .noteq})) |op| {
        const parent: Ast = .{ .expr = .{
            .token = op,
            .left = left,
            .right = try self.equality(),
            .terminated = self.lexer.consume_if_eq(&[_]enums.Tag{.semicolon}) 
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}

fn relational(self: *@This()) anyerror!*Ast {
    var left = try self.shift();
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.lt, .lteq, .gt, .gteq})) |op| {
        const parent: Ast = .{ .expr = .{
            .token = op,
            .left = left,
            .right = try self.relational(),
            .terminated = self.lexer.consume_if_eq(&[_]enums.Tag{.semicolon}) 
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn shift(self: *@This()) anyerror!*Ast {
    var left = try self.additive();
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.shl, .shr})) |op| {
        const parent: Ast = .{ .expr = .{
            .token = op,
            .left = left,
            .right = try self.shift(),
            .terminated = self.lexer.consume_if_eq(&[_]enums.Tag{.semicolon}) 
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}

fn additive(self: *@This()) anyerror!*Ast {
    var left = try self.multiplicative();
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.plus, .minus})) |op| {
        const parent: Ast = .{ .expr = .{
            .token = op,
            .left = left,
            .right = try self.additive(),
            .terminated = self.lexer.consume_if_eq(&[_]enums.Tag{.semicolon}) 
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn multiplicative(self: *@This()) anyerror!*Ast {
    var left = try self.unary();
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.star, .slash, .percent})) |op| {
        const parent: Ast = .{ .expr = .{
            .token = op,
            .left = left,
            .right = try self.multiplicative(),
            .terminated = self.lexer.consume_if_eq(&[_]enums.Tag{.semicolon}) 
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn unary(self: *@This()) anyerror!*Ast {
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.plus, .minus, .bang, .tilde, .star, .amp})) |op| {
        const out: Ast = .{ .expr = .{
            .token = op,
            .left = try self.unary(),
            .right = null,
            .terminated = self.lexer.consume_if_eq(&[_]enums.Tag{.semicolon}) 
        }};
        return try mem.createWith(self.gpa, out);
    }
    return try self.primary();
}
fn primary(self: *@This()) anyerror!*Ast {
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.ident, .float_literal, .int_literal, .string_literal, .raw_string_literal, .char_literal, .keyword_true, .keyword_false})) |lit| {
        const out: Ast = Ast.create_token(lit, self.lexer.consume_if_eq(&[_]enums.Tag{.semicolon}) );
        return try mem.createWith(self.gpa, out);
    }
    if (self.lexer.consume_if_eq(&[_]enums.Tag{.open_paren})) |_| {
        const out = try self.expression();
        _ = try self.lexer.expect_token(.close_paren);
        return out;
    }
    return error.Invalid;
}




