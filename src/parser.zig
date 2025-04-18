//! This file contains the parser for the conical language, it does not do semantic analysis
const std = @import("std");

const Ast = @import("Ast.zig").Ast;
const AstNode = @import("Ast.zig").AstNode;
const AstTypes = @import("Ast.zig");
const Block = @import("Ast.zig").Block;
const diag = @import("diag.zig");
const lex = @import("lexer.zig");
const mem = @import("mem.zig");
const types = @import("types.zig");

//FIX: diagnostic spans need to be better
gpa: std.mem.Allocator,
lexer: lex.Lexer,
session: *diag.Session,

/// Initialize the parser from an already existing Lexer isntance
pub fn init_from_lexer(in: lex.Lexer, session: *diag.Session, gpa: std.mem.Allocator) @This() {
    return .{
        .lexer = in,
        .gpa = gpa,
        .session = session
    };
}

/// Initialize the parser from source
pub fn init_from_source(src: []const u8, session: *diag.Session, gpa: std.mem.Allocator) @This() {
    return .{
        .lexer = lex.Lexer.init(src),
        .gpa = gpa,
        .session = session
    };
}

/// The entrypoint for the parser
pub fn parse(self: *@This()) ![]*Ast {
    var ast = std.ArrayList(*Ast).init(self.gpa);
    while (self.lexer.has_next()) {
        const as = self.stmt() catch |err| {
            if (err == error.EOF) {
                break;
            }
            return err;
        };
        try ast.append(as);
    }
    return ast.toOwnedSlice();
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
    } else {
        try self.session.emit(.Error, self.lexer.next_token().span, "Unexepected token ret");
        return error.UnexpectedToken;
    }
}



// a required block 
// block = "{" stmt* "}"
fn block(self: *@This()) !*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index
    };
    if (self.lexer.consume_if_eq(&[_]types.Tag{.open_bracket})) |_| {
        span.start = self.lexer.index;
        span.end = self.lexer.index;
        var exprs = std.ArrayList(*Ast).init(self.gpa);
        while (self.lexer.has_next() and !self.lexer.is_next_token(.close_bracket)) {
            const exp = try self.stmt();
            if (self.lexer.consume_if_eq(&[_]types.Tag{.semicolon})) |_| {
                const termnt: Ast = Ast.create(.{ .terminated = exp }, span);
                try exprs.append(try mem.createWith(self.gpa, termnt)); 
            } else {
                try exprs.append(exp);
            }
        } else if (!self.lexer.has_next()) {
            try self.session.emit(.Error, span, "Unclosed curly-bracket");
            return error.UnmatchedBracket;
        }
        _ = self.lexer.next_token();
        span.end = self.lexer.index;
        const out: Ast = Ast.create(.{ .block = .{ .exprs = try exprs.toOwnedSlice() }}, span);
        return try mem.createWith(self.gpa, out);
    } 
    try self.session.emit(.Error, span, "Expected a block");
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
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index
    };
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
                self.expect(.close_square) catch |err| {
                    try self.session.emit(.Error, span, "Expected a closing square bracket '['");
                    return err;
                };
            },
            else => unreachable
        }
        try modifiers.append(mod);
    }
    var base_ty: AstTypes.Type = .{.base_type = .{ .primitive = .Unit}, .modifiers = if (modifiers.items.len > 0) try modifiers.toOwnedSlice() else null };
    if (self.lexer.consume_if_eq(&[_]types.Tag{.ident, .open_paren})) |ty| {
        if (ty.tag == .open_paren) {
            if (self.lexer.consume_if_eq(&[_]types.Tag{.close_paren})) |_| {
                if (base_ty.modifiers != null) {
                    span.end = self.lexer.index;
                    try self.session.emit(.Error, span, "Unit type cannot have modifiers");
                    return error.UnitCannotHaveMods;
                }
            } else {
                try self.session.emit(.Error, span, "Types cannot currently have paranthesis other than unit");
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
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };
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
        self.expect(.open_paren) catch |err| {
            try self.session.emit(.Error, span, "Expected an open parenthesis");
            return err;
        };
        while (!self.lexer.is_next_token(.close_paren)) {
            const tok = self.lexer.next_token();
            if (tok.tag == .ident) {
                try params.append(.{ .span = tok.span });
            } else if (tok.tag == .comma) {
                if (self.lexer.is_next_token(.close_paren)) {
                    try self.session.emit(.Error, span, "Parameter list contains an extra comma");
                    return error.UnfinishedParameterList;
                }
            } else {
                try self.session.emit(.Error, span, "Function signature contains an invalid token");
                return error.InvalidTokenInFunctionSignature;
            }
        }
        _ = self.lexer.next_token();
        var param_types = std.ArrayList(AstTypes.Type).init(self.gpa);
        if (self.lexer.consume_if_eq(&[_]types.Tag{.colon})) |_| {
            if (self.lexer.consume_if_eq(&[_]types.Tag{.open_paren})) |_| {
                while (!self.lexer.is_next_token(.close_paren)) {
                    try param_types.append(try self.parse_type());
                    if (self.lexer.consume_if_eq(&[_]types.Tag{.comma})) |_| {
                        if (self.lexer.is_next_token(.close_paren)) {
                            try self.session.emit(.Error, span, "Type parameter list contains an extra comma");
                            return error.UnfinishedTypeParamaterList;
                        }
                    }
                }
                _ = self.lexer.next_token();
            } else {
                try param_types.append(try self.parse_type());
            }
        }
        if (param_types.items.len != params.items.len) {
            span.end = self.lexer.index;
            try self.session.emit(.Error, span, try std.fmt.allocPrint(self.gpa, "Type list is shorter than the parameter list. Expected: {}, got: {}", .{params.items.len, param_types.items.len}));
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
        span.end = self.lexer.index;
        const out: Ast = Ast.create(.{ .fn_decl = .{
            .ident = .{ .span = ident.span },
            .params = try params.toOwnedSlice(),
            .param_types = try param_types.toOwnedSlice(),
            .return_ty = return_ty,
            .decl_mod = decl_mod,
            .fn_mod = fn_mod,
            .body = blck
        }}, span);
        return try mem.createWith(self.gpa, out);
    }
    return self.var_decl();
}

// parses a variable decleration
// var_decl = ("let" | "mut") (":" type)? ("=" expression)? ";"?
fn var_decl(self: *@This()) !*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    if (self.lexer.consume_if_eq(&[_]types.Tag{.keyword_let, .keyword_mut})) |key| {
        const ident = try self.expect_ret(.ident);
        var ty: ?AstTypes.Type = null;
        if (self.lexer.consume_if_eq(&[_]types.Tag{.colon})) |_| {
            ty = try self.parse_type();
        }
        var initial: ?*Ast = null;
        if (self.lexer.consume_if_eq(&[_]types.Tag{.eq})) |_| {
            span.start = self.lexer.index;
            initial = try self.expression();
            self.expect(.semicolon) catch |err| {
                span.end = self.lexer.index;
                try self.session.emit(.Error, span, "Expected a semicolon at the end of assignment");
                return err;
            };
        }
        span.end = self.lexer.index;
        var out: Ast = Ast.create(.{ .var_decl = .{
            .is_mut = key.tag == .keyword_mut,
            .ident = .{ .span = ident.span },
            .ty = ty,
            .initialize = initial
        }}, span);
        if (self.lexer.consume_if_eq(&[_]types.Tag{.semicolon})) |_| {
            span.end = self.lexer.index;
            out = Ast.create(.{ .terminated = try mem.createWith(self.gpa, out)}, span);
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

    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };


    if (self.lexer.consume_if_eq(&[_]types.Tag{.keyword_if})) |_| {
        const condition = try self.expression();
        const then_block = try self.optional_block();
        var else_block: ?*Ast = null;
        if (self.lexer.consume_if_eq(&[_]types.Tag{.keyword_else})) |_| {
            else_block = try self.optional_block();
        }
        span.end = self.lexer.index;
        const out: Ast = Ast.create(.{ .if_stmt = .{
            .condition = condition,
            .block = then_block,
            .else_block = else_block
        }}, span);
        return try mem.createWith(self.gpa, out);
    }
    return try self.while_loop();
}

fn while_loop(self: *@This()) !*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    if (self.lexer.consume_if_eq(&[_]types.Tag{.keyword_while})) |_| {
        const condition = try self.expression();
        const body = try self.block();
        span.end = self.lexer.index;
        const out: Ast = Ast.create(.{ .while_loop = .{
            .condition = condition,
            .block = body
        }}, span);
        return try mem.createWith(self.gpa, out);
    }
    return try self.assignment();
}

fn assignment(self: *@This()) anyerror!*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    const lval = try self.ternary();
    if (self.lexer.consume_if_eq(&[_]types.Tag{
        .eq, .pluseq, .minuseq, .stareq, .slasheq, .percenteq, .shleq, .shreq, .ampeq, .careteq, .pipeeq
    })) |token| {
        span.end = self.lexer.index;
        var parent: Ast = Ast.create(.{ .assignment = .{
            .op = token,
            .lvalue = lval,
            .expr = try self.expression(),
        }}, span);
        if (self.lexer.consume_if_eq(&[_]types.Tag{.semicolon})) |_| {
            span.end = self.lexer.index;
            parent = Ast.create(.{ .terminated = try mem.createWith(self.gpa, parent) }, span);
        }
        return try mem.createWith(self.gpa, parent);
    } 
    return lval;
}

fn ternary(self: *@This()) !*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var condition = try self.logical_or();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.question})) |_| {
        const true_path = try self.expression();
        self.expect(.colon) catch |err| {
            span.end = self.lexer.index;
            try self.session.emit(.Error, span, "Expected ':'");
            return err;
        };
        const false_path = try self.ternary();
        span.end = self.lexer.index;
        const parent: Ast = Ast.create(.{ .ternary = .{
            .condition = condition,
            .true_path = true_path,
            .false_path = false_path,
        }}, span);
        condition = try mem.createWith(self.gpa, parent);
    }
    return condition;
}

fn logical_or(self: *@This()) !*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.logical_and();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.pipe2})) |pipe2| {
        span.end = self.lexer.index;
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = pipe2,
            .left = left,
            .right = try self.logical_or(),
        }}, span);
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn logical_and(self: *@This()) !*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.bitwise_or();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.amp2})) |amp2| {
        span.end = self.lexer.index;
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = amp2,
            .left = left,
            .right = try self.logical_and(),
        }}, span);
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}

fn bitwise_or(self: *@This()) !*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.bitwise_xor();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.pipe})) |pipe| {
        span.end = self.lexer.index;
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = pipe,
            .left = left,
            .right = try self.bitwise_or(),
        }}, span);
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn bitwise_xor(self: *@This()) anyerror!*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.bitwise_and();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.caret})) |caret| {
        span.end = self.lexer.index;
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = caret,
            .left = left,
            .right = try self.bitwise_xor(),
        }}, span);
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn bitwise_and(self: *@This()) anyerror!*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.equality();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.amp})) |amp| {
        span.end = self.lexer.index;
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = amp,
            .left = left,
            .right = try self.bitwise_and(),
        }}, span);
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}

fn equality(self: *@This()) anyerror!*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.relational();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.eq2, .noteq})) |op| {
        span.end = self.lexer.index;
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = try self.equality(),
        }}, span);
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}

fn relational(self: *@This()) anyerror!*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.shift();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.lt, .lteq, .gt, .gteq})) |op| {
        span.end = self.lexer.index;
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = try self.relational(),
        }}, span);
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn shift(self: *@This()) anyerror!*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.additive();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.shl, .shr})) |op| {
        span.end = self.lexer.index;
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = try self.shift(),
        }}, span);
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}

fn additive(self: *@This()) anyerror!*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.multiplicative();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.plus, .minus})) |op| {
        span.end = self.lexer.index;
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = try self.additive(),
        }}, span);
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn multiplicative(self: *@This()) anyerror!*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.unary();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.star, .slash, .percent})) |op| {
        span.end = self.lexer.index;
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = try self.multiplicative(),
        }}, span);
        left = try mem.createWith(self.gpa, parent);
    }
    return left;
}
fn unary(self: *@This()) anyerror!*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    if (self.lexer.consume_if_eq(&[_]types.Tag{.minus, .bang, .tilde, .star, .amp})) |op| {
        span.end = self.lexer.index;
        const out: Ast = Ast.create(.{ .unary_expr = .{
            .op = op,
            .expr = try self.unary(),
        }}, span);
        return try mem.createWith(self.gpa, out);
    }
    return try self.primary();
}
fn primary(self: *@This()) anyerror!*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index + 1,
    };

    if (self.lexer.consume_if_eq(&[_]types.Tag{.ident, .float_literal, .int_literal, .string_literal, .raw_string_literal, .char_literal, .keyword_true, .keyword_false})) |lit| {
        span.end = self.lexer.index;
        const out: Ast = Ast.create(.{ .terminal = lit }, span);
        return try mem.createWith(self.gpa, out);
    }
    if (self.lexer.consume_if_eq(&[_]types.Tag{.open_paren})) |_| { //TODO: add support for unit ()
        const out = try self.expression();
        self.expect(.close_paren) catch |err| {
            try self.session.emit(.Error, span, "Expected a closing parenthesis");
            return err;
        };
        return out;
    }
    if (self.lexer.is_next_token(.open_bracket)) {
        return try self.block();
    }
    if (self.lexer.is_next_token(.eof)) {
        return error.EOF;
    }
    try self.session.emit(.Error, span, try std.fmt.allocPrint(self.gpa, "Unexpected token: {any}", .{self.lexer.next_token()}));
    return error.Invalid;
}




