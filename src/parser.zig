//! This file contains the parser for the conical language, it does not do semantic analysis
const std = @import("std");
const lex = @import("lexer.zig");
const types = @import("types.zig");
const Ast = @import("Ast.zig").Ast;
const Block = @import("Ast.zig").Block;
const AstTypes = @import("Ast.zig");
const mem = @import("mem.zig");


gpa: std.mem.Allocator,
lexer: lex.Lexer,

/// Initialize the parser from an already existing Lexer isntance
pub fn init_from_lexer(in: lex.Lexer, gpa: std.mem.Allocator) @This() {
    return .{
        .lexer = in,
        .gpa = gpa
    };
}

/// Initialize the parser from source
pub fn init_from_source(src: []const u8, gpa: std.mem.Allocator) @This() {
    return .{
        .lexer = lex.Lexer.init(src),
        .gpa = gpa
    };
}

/// The entrypoint for the parser
pub fn parse(self: *@This()) !*Ast {
    var ast: *Ast = undefined;
    while (self.lexer.has_next()) {
        ast = try self.fn_decl();
    }
    return ast;
}


// expect a token type does not return anything
fn expect(self: *@This(), token: types.Tag) !void {
    if (self.lexer.consume_if_eq(&[_]types.Tag{token})) |_| {
        return;
    } else {
        return error.UnexpectedToken;
    }
}

// expect a token type and return it if it is valid
fn expect_ret(self: *@This(), token: types.Tag) !types.Token {
    if (self.lexer.consume_if_eq(&[_]types.Tag{token})) |tok| {
        return tok;
    }
    return error.UnexpectedToken;
}



// a required block 
// block = "{" stmt* "}"
fn block(self: *@This()) !*Ast {
    if (self.lexer.consume_if_eq(&[_]types.Tag{.open_bracket})) |_| {
        var exprs = std.ArrayList(*Ast).init(self.gpa);
        while (self.lexer.has_next() and !self.lexer.is_next_token(.close_bracket)) {
            const exp = try self.stmt();
            if (self.lexer.consume_if_eq(&[_]types.Tag{.semicolon})) |_| {
                const termnt: Ast = .{ .terminated = exp };
                try exprs.append(try mem.createWith(self.gpa, termnt)); 
            } else {
                try exprs.append(exp);
            }
        } else if (!self.lexer.has_next()) {
            return error.UnmatchedBracket;
        }
        try self.expect(.close_bracket);
        const out: Ast = .{ .block = .{ .exprs = try exprs.toOwnedSlice() }};
        return try mem.createWith(self.gpa, out);
    } 
    return error.RequiredBlock;
}

// an optional block
// opt_block = block | stmt
fn optional_block(self: *@This()) !*Ast {
    if (self.lexer.is_next_token(.open_bracket)) {
        return self.block();
    }
    return self.stmt();
}

// parse a type with modifiers and primitives
// type = typemods* (primitivetype | ident)
fn parse_type(self: *@This()) !AstTypes.Type {
    var modifiers = std.ArrayList(AstTypes.TypeModifier).init(self.gpa);
    while (self.lexer.consume_if_eq(&[_]types.Tag{.amp, .amp2, .star, .open_square, .keyword_mut, .keyword_const})) |mmod| {
        var mod: AstTypes.TypeModifier = .Ref;
        switch (mmod.tag) {
            .amp => mod = .Ref,
            .amp2 => {
                mod = .Ref;
                try modifiers.append(mod);
            },
            .star => mod = .Ptr,
            .keyword_mut => {
                if (modifiers.getLast() == .Ref) {
                    modifiers.items[modifiers.items.len - 1] = .RefMut;
                } else if (modifiers.getLast() == .Ptr) {
                    modifiers.items[modifiers.items.len - 1] = .PtrMut;
                } else {
                    mod = .Mut;
                    try modifiers.append(mod);
                }
                break;
            },
            .keyword_const => {
                if (modifiers.getLast() == .Ref) {
                    modifiers.items[modifiers.items.len - 1] = .RefConst;
                } else if (modifiers.getLast() == .Ptr) {
                    modifiers.items[modifiers.items.len - 1] = .PtrConst;
                } else {
                    mod = .Const;
                    try modifiers.append(mod);
                }
                break;
            },
            .open_square => {
                if (self.lexer.consume_if_eq(&[_]types.Tag{.int_literal})) |lit| {
                    mod = .{ .Array = lit };
                } else {
                    mod = .Slice;
                }
                try self.expect(.close_square);
            },
            else => unreachable
        }
        try modifiers.append(mod);
    }
    var base_ty: AstTypes.Type = .{.base_type = .{ .primitive = .Unit}, .modifiers = if (modifiers.items.len > 0) try modifiers.toOwnedSlice() else null };
    if (self.lexer.consume_if_eq(&[_]types.Tag{.ident, .open_paren})) |ty| {
        if (ty.tag == .open_paren) {
            if (self.lexer.consume_if_eq(&[_]types.Tag{.close_paren})) |_| {
                if (modifiers.items.len != 0) {
                    return error.UnitCannotHaveMods;
                }
            } else {
                return error.ParenInTypeExprNotUnit;
            }
        } else {
            if (AstTypes.PrimitiveType.prims.get(ty.span.get_string(self.lexer.buffer))) |val| {
                base_ty.base_type = .{ .primitive = val };
            } else {
                base_ty.base_type = .{ .user = .{ .span = ty.span } };
            }
        }
    }
    return base_ty;
}

// trys to parse a global decleration modifier otherwise returns null
fn try_decl_mod(self: *@This()) ?AstTypes.GlobalDeclMod {
    if (self.lexer.consume_if_eq(&[_]types.Tag{.keyword_pub, .keyword_export, .keyword_extern})) |key| {
        return switch (key.tag) {
            .keyword_pub => .Pub,
            .keyword_export => .Export,
            .keyword_extern => .Extern,
            else => unreachable,
        };
    }
    return null;
}



// parses a function decleration
// fn_decl = decl_mod? fn_mod? "fn" ident "(" param* ")" (":" "(" (type ",") | type ")" )? ("->" type)? block?
fn fn_decl(self: *@This()) !*Ast {
    var decl_mod: ?AstTypes.GlobalDeclMod = null;
    var fn_mod: ?AstTypes.FnModifier = null;
    if (self.try_decl_mod()) |decl| {
        decl_mod = decl;
    }
    if (self.lexer.consume_if_eq(&[_]types.Tag{.keyword_async, .keyword_pure, .keyword_comp})) |fnm| {
        fn_mod = switch (fnm.tag) {
            .keyword_async => .Async,
            .keyword_pure => .Pure,
            .keyword_comp => .CompTime,
            else => unreachable
        };
    }
    if (self.lexer.consume_if_eq(&[_]types.Tag{.keyword_fn})) |_| {
        var params = std.ArrayList(AstTypes.Ident).init(self.gpa);
        const ident = try self.expect_ret(.ident);
        try self.expect(.open_paren);
        while (!self.lexer.is_next_token(.close_paren)) {
            const tok = self.lexer.next_token();
            if (tok.tag == .ident) {
                try params.append(.{ .span = tok.span });
            } else if (tok.tag == .comma) {
            } else {
                return error.InvalidTokenInFunctionSignature;
            }
        }
        _ = self.lexer.next_token();
        var param_types = std.ArrayList(AstTypes.Type).init(self.gpa);
        if (self.lexer.consume_if_eq(&[_]types.Tag{.colon})) |_| {
            try self.expect(.open_paren);
            while (!self.lexer.is_next_token(.close_paren)) {
                try param_types.append(try self.parse_type());
                _ = self.lexer.consume_if_eq(&[_]types.Tag{.comma});
            }
            _ = self.lexer.next_token();
        }
        if (param_types.items.len != params.items.len) {
            return error.MismatchedParamToTypeLen;
        }
        var return_ty: AstTypes.Type = .{
            .base_type = .{ .primitive = .Unit },
            .modifiers = null
        };
        if (self.lexer.consume_if_eq(&[_]types.Tag{.thin_arrow})) |_| {
            return_ty = try self.parse_type();
        }
        var blck: ?*Ast = null;
        if (!self.lexer.is_next_token(.semicolon)) {
            blck = try self.block();
        } else {
            _ = self.lexer.next_token();
        }
        const out: Ast = .{ .fn_decl = .{
            .ident = .{ .span = ident.span },
            .params = try params.toOwnedSlice(),
            .param_types = try param_types.toOwnedSlice(),
            .return_ty = return_ty,
            .decl_mod = decl_mod,
            .fn_mod = fn_mod,
            .body = blck
        }};
        return try mem.createWith(self.gpa, out);
    }
    return self.var_decl();
}

// parses a variable decleration
// var_decl = ("let" | "mut") (":" type)? ("=" expression)? ";"?
fn var_decl(self: *@This()) !*Ast {
    if (self.lexer.consume_if_eq(&[_]types.Tag{.keyword_let, .keyword_mut})) |key| {
        const ident = try self.expect_ret(.ident);
        var ty: ?AstTypes.Type = null;
        if (self.lexer.consume_if_eq(&[_]types.Tag{.colon})) |_| {
            ty = try self.parse_type();
        }
        var initial: ?*Ast = null;
        if (self.lexer.is_next_token(.eq)) {
            initial = try self.assignment();
        }
        var out: Ast = .{ .var_decl = .{
            .is_mut = key.tag == .keyword_mut,
            .ident = .{ .span = ident.span },
            .ty = ty,
            .initialize = initial
        }};
        if (self.lexer.consume_if_eq(&[_]types.Tag{.semicolon})) |_| {
            out = .{ .terminated = try mem.createWith(self.gpa, out) };
        }
        return try mem.createWith(self.gpa, out);
    }
    return try self.ifstmt();
}

fn stmt(self: *@This()) anyerror!*Ast {
    return self.fn_decl();
}

fn expression(self: *@This()) anyerror!*Ast {
    return self.ifstmt();
}

fn ifstmt(self: *@This()) !*Ast {
    if (self.lexer.consume_if_eq(&[_]types.Tag{.keyword_if})) |_| {
        const condition = try self.expression();
        const then_block = try self.optional_block();
        var else_block: ?*Ast = null;
        if (self.lexer.consume_if_eq(&[_]types.Tag{.keyword_else})) |_| {
            else_block = try self.optional_block();
        }
        const out: Ast = .{ .if_stmt = .{
            .condition = condition,
            .block = then_block,
            .else_block = else_block
        }};
        return try mem.createWith(self.gpa, out);
    }
    return try self.assignment();
}

fn assignment(self: *@This()) anyerror!*Ast {
    if (self.lexer.consume_if_eq(&[_]types.Tag{
        .eq, .pluseq, .minuseq, .stareq, .slasheq, .percenteq, .shleq, .shreq, .ampeq, .careteq, .pipeeq
    })) |token| {
        var parent: Ast = .{ .assignment = .{
            .op = token,
            .expr = try self.ternary(),
        }};
        if (self.lexer.consume_if_eq(&[_]types.Tag{.semicolon})) |_| {
            parent = .{ .terminated = try mem.createWith(self.gpa, parent) };
        }
        return try mem.createWith(self.gpa, parent);
    } 
    return self.ternary();
}

fn ternary(self: *@This()) !*Ast {
    var condition = try self.logical_or();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.question})) |_| {
        const true_path = try self.expression();
        _ = try self.expect(.colon);
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
    if (self.lexer.consume_if_eq(&[_]types.Tag{.pipe2})) |pipe2| {
        const parent: Ast = .{ .binary_expr = .{
            .op = pipe2,
            .left = left,
            .right = try self.logical_or(),
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn logical_and(self: *@This()) !*Ast {
    var left = try self.bitwise_or();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.amp2})) |amp2| {
        const parent: Ast = .{ .binary_expr = .{
            .op = amp2,
            .left = left,
            .right = try self.logical_and(),
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}

fn bitwise_or(self: *@This()) !*Ast {
    var left = try self.bitwise_xor();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.pipe})) |pipe| {
        const parent: Ast = .{ .binary_expr = .{
            .op = pipe,
            .left = left,
            .right = try self.bitwise_or(),
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn bitwise_xor(self: *@This()) anyerror!*Ast {
    var left = try self.bitwise_and();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.caret})) |caret| {
        const parent: Ast = .{ .binary_expr = .{
            .op = caret,
            .left = left,
            .right = try self.bitwise_xor(),
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn bitwise_and(self: *@This()) anyerror!*Ast {
    var left = try self.equality();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.amp})) |amp| {
        const parent: Ast = .{ .binary_expr = .{
            .op = amp,
            .left = left,
            .right = try self.bitwise_and(),
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}

fn equality(self: *@This()) anyerror!*Ast {
    var left = try self.relational();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.eq2, .noteq})) |op| {
        const parent: Ast = .{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = try self.equality(),
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}

fn relational(self: *@This()) anyerror!*Ast {
    var left = try self.shift();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.lt, .lteq, .gt, .gteq})) |op| {
        const parent: Ast = .{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = try self.relational(),
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn shift(self: *@This()) anyerror!*Ast {
    var left = try self.additive();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.shl, .shr})) |op| {
        const parent: Ast = .{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = try self.shift(),
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}

fn additive(self: *@This()) anyerror!*Ast {
    var left = try self.multiplicative();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.plus, .minus})) |op| {
        const parent: Ast = .{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = try self.additive(),
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn multiplicative(self: *@This()) anyerror!*Ast {
    var left = try self.unary();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.star, .slash, .percent})) |op| {
        const parent: Ast = .{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = try self.multiplicative(),
        }};
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn unary(self: *@This()) anyerror!*Ast {
    if (self.lexer.consume_if_eq(&[_]types.Tag{.plus, .minus, .bang, .tilde, .star, .amp, .semicolon})) |op| {
        const out: Ast = .{ .unary_expr = .{
            .op = op,
            .expr = try self.unary(),
        }};
        return try mem.createWith(self.gpa, out);
    }
    return try self.primary();
}
fn primary(self: *@This()) anyerror!*Ast {
    if (self.lexer.consume_if_eq(&[_]types.Tag{.ident, .float_literal, .int_literal, .string_literal, .raw_string_literal, .char_literal, .keyword_true, .keyword_false})) |lit| {
        const out: Ast = .{ .terminal = lit };
        return try mem.createWith(self.gpa, out);
    }
    if (self.lexer.consume_if_eq(&[_]types.Tag{.open_paren})) |_| {
        const out = try self.expression();
        _ = try self.expect(.close_paren);
        return out;
    }
    std.debug.print("Invalid: {s}\n", .{self.lexer.next_token().span.get_string(self.lexer.buffer)});
    return error.Invalid;
}




