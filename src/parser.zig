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

//FIX: diagnostic messages need to be better
gpa: std.mem.Allocator,
lexer: lex.Lexer,
session: *diag.Session,
type_map: *types.TypeTbl,

/// Initialize the parser from an already existing Lexer isntance
pub fn init_from_lexer(in: lex.Lexer, session: *diag.Session, type_map: *types.TypeTbl, gpa: std.mem.Allocator) @This() {
    return .{
        .lexer = in,
        .gpa = gpa,
        .session = session,
        .type_map = type_map,
    };
}

/// Initialize the parser from source
pub fn init_from_source(src: []const u8, session: *diag.Session, type_map: *types.TypeTbl, gpa: std.mem.Allocator) @This() {
    return .{
        .lexer = lex.Lexer.init(src),
        .gpa = gpa,
        .session = session,
        .type_map = type_map,
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
        try self.session.emit(.Error, self.lexer.next_token().span, "Unexepected token");
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
        var exprs = std.ArrayList(*Ast).init(self.gpa);
        while (self.lexer.has_next() and !self.lexer.is_next_token(.close_bracket)) {
            const exp = try self.stmt();
            span.merge(exp.span);
            if (self.lexer.consume_if_eq(&[_]types.Tag{.semicolon})) |_| {
                const termnt: Ast = Ast.create(.{ .terminated = exp }, span);
                span.end = self.lexer.index;
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
    return self.expression();
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
    var base_ty: AstTypes.Type = .createPrimitive(.Unit, if (modifiers.items.len > 0) try modifiers.toOwnedSlice() else null );
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
        } else if (AstTypes.PrimitiveType.prims.get(ty.span.get_string(self.lexer.buffer))) |val| {
            base_ty.base_type = .{ .primitive = val };
        }  else {
            base_ty.base_type = .{ .user = .{ .span = ty.span, .value = ty.span.get_string(self.lexer.buffer) } };
        }
    } else if (self.lexer.consume_if_eq(&[_]types.Tag{.keyword_struct})) |_|{
        try self.expect(.open_bracket);
        var fields = std.StringHashMap(AstTypes.TypeId).init(self.gpa);

        while (!self.lexer.is_next_token(.close_bracket)) {
            const ident = try self.expect_ret(.ident);
            try self.expect(.colon);
            const field_type = try self.parse_type();
            try self.expect(.semicolon); //NOTE: could possibly change to commas, not sure
            fields.putNoClobber(ident.span.get_string(self.lexer.buffer), field_type.hash()) catch {
                span.merge(.{ .start = span.start, .end = self.lexer.index});
                try self.session.emit(.Error, span, "Struct with duplicate fields");
                return error.DuplicateFields;
            };
        }
        try self.expect(.close_bracket);
        base_ty.base_type = .{ .strct = .{ .fields = fields } };
    } else if (self.lexer.consume_if_eq(&[_]types.Tag{.keyword_enum})) |_| {
        try self.expect(.open_bracket);
        var variants = std.StringHashMap(?AstTypes.TypeId).init(self.gpa);

        while (!self.lexer.is_next_token(.close_bracket)) {
            const ident = try self.expect_ret(.ident);
            var var_ty: ?AstTypes.TypeId = null;
            if (self.lexer.consume_if_eq(&[_]types.Tag{.colon})) |_| {
                var_ty = (try self.parse_type()).hash();
            }
            if (!self.lexer.is_next_token(.comma) and !self.lexer.is_next_token(.close_bracket)) {
                span.merge(.{ .start = span.start, .end = self.lexer.index});
                try self.session.emit(.Error, span, "Missing comma");
                return error.MissingCommaInEnum;
            }
            _ = self.lexer.consume_if_eq(&[_]types.Tag{.comma});
            variants.putNoClobber(ident.span.get_string(self.lexer.buffer), var_ty) catch {
                span.merge(.{ .start = span.start, .end = self.lexer.index});
                try self.session.emit(.Error, span, "Duplicate enum variant");
                return error.DuplicateEnumVariant;
            };
        }
        try self.expect(.close_bracket);
        base_ty.base_type = .{ .@"enum" = .{ .variants = variants } };
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
            span.merge(.{.start = span.start, .end = self.lexer.index});
            try self.session.emit(.Error, span, "Expected an open parenthesis");
            return err;
        };
        while (!self.lexer.is_next_token(.close_paren)) {
            const tok = self.lexer.next_token();
            if (tok.tag == .ident) {
                try params.append(.{ .span = tok.span, .value = tok.span.get_string(self.lexer.buffer)});
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
        var param_types = std.ArrayList(AstTypes.TypeId).init(self.gpa);
        if (self.lexer.consume_if_eq(&[_]types.Tag{.colon})) |_| {
            if (self.lexer.consume_if_eq(&[_]types.Tag{.open_paren})) |_| {
                while (!self.lexer.is_next_token(.close_paren)) {
                    try param_types.append((try self.parse_type()).hash());
                    if (self.lexer.consume_if_eq(&[_]types.Tag{.comma})) |_| {
                        if (self.lexer.is_next_token(.close_paren)) {
                            try self.session.emit(.Error, span, "Type parameter list contains an extra comma");
                            return error.UnfinishedTypeParamaterList;
                        }
                    }
                }
                _ = self.lexer.next_token();
            } else {
                try param_types.append((try self.parse_type()).hash());
            }
        }
        span.merge(.{.start = span.start, .end = self.lexer.index});
        if (param_types.items.len != params.items.len) {
            try self.session.emit(.Error, span, try std.fmt.allocPrint(self.gpa, "Type list is shorter than the parameter list. Expected: {}, got: {}", .{params.items.len, param_types.items.len}));
            return error.MismatchedParamToTypeLen;
        }
        var return_ty: AstTypes.Type = AstTypes.Type.createPrimitive(.Unit, null);
        if (self.lexer.consume_if_eq(&[_]types.Tag{.thin_arrow})) |_| {
            return_ty = try self.parse_type();
            span.merge(.{.start = span.start, .end = self.lexer.index});
        }
        var blck: ?*Ast = null;
        if (!self.lexer.is_next_token(.semicolon)) {
            blck = try self.block();
            span.merge(blck.?.span);
        } else {
            _ = self.lexer.next_token();
        }
        const retid = return_ty.hash();
        _ = try self.type_map.getOrPutValue(retid, return_ty);
        const out: Ast = Ast.create(.{ .fn_decl = .{
            .ident = .{ .span = ident.span, .value = ident.span.get_string(self.lexer.buffer)},
            .params = try params.toOwnedSlice(),
            .param_types = try param_types.toOwnedSlice(),
            .return_ty = retid,
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
        span.merge(ident.span);
        span.merge(key.span);
        var ty: ?AstTypes.Type = null;
        if (self.lexer.consume_if_eq(&[_]types.Tag{.colon})) |_| {
            ty = try self.parse_type();
            span.merge(.{.start = span.start, .end = self.lexer.index});
        }
        var initial: ?*Ast = null;
        if (self.lexer.consume_if_eq(&[_]types.Tag{.eq})) |_| {
            initial = try self.expression();
            span.merge(initial.?.span);
            self.expect(.semicolon) catch |err| {
                try self.session.emit(.Error, span, "Expected a semicolon at the end of assignment");
                return err;
            };
        }
        var tyid: ?u64 = null;
        if (ty) |tyy| {
            tyid = tyy.hash();
            _ = try self.type_map.getOrPutValue(tyid.?, tyy);
        }
        var out: Ast = Ast.create(.{ .var_decl = .{
            .is_mut = key.tag == .keyword_mut,
            .ident = .{ .span = ident.span, .value = ident.span.get_string(self.lexer.buffer)},
            .ty = tyid,
            .initialize = initial
        }}, span);
        if (self.lexer.consume_if_eq(&[_]types.Tag{.semicolon})) |tok| {
            span.merge(tok.span);
            out = Ast.create(.{ .terminated = try mem.createWith(self.gpa, out)}, span);
        }
        return try mem.createWith(self.gpa, out);
    }
    return try self.type_decl();
}

fn type_decl(self: *@This()) !*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    if (self.lexer.consume_if_eq(&[_]types.Tag{.keyword_type})) |_| {
        const ident = try self.expect_ret(.ident);
        try self.expect(.eq);
        const ty = try self.parse_type();
        try self.expect(.semicolon);
        const tyid = ty.hash();
        span.end = self.lexer.index;
        _ = try self.type_map.getOrPutValue(tyid, ty);
        const out: Ast = Ast.create(.{ .type_decl = .{
            .ident = .{ 
                .span = ident.span,
                .value = ident.span.get_string(self.lexer.buffer)
            },
            .ty = tyid,
        }}, span);
        return try mem.createWith(self.gpa, out);
    }
    return self.expression();

}

fn stmt(self: *@This()) anyerror!*Ast {
    return self.fn_decl();
}

fn expression(self: *@This()) anyerror!*Ast {
    return self.return_stmt();
}
fn return_stmt(self: *@This()) !*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };
    if (self.lexer.consume_if_eq(&[_]types.Tag{.keyword_return})) |tok| {
        span.merge(tok.span);
        const expr = try self.expression();
        span.merge(expr.span);
        const out = Ast.create(.{ .return_stmt = expr }, span);
        return try mem.createWith(self.gpa, out);
    }
    return self.ifstmt();
}
fn ifstmt(self: *@This()) !*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };
    if (self.lexer.consume_if_eq(&[_]types.Tag{.keyword_if})) |tok| {
        span.merge(tok.span);
        const condition = try self.expression();
        span.merge(condition.span);
        const then_block = try self.optional_block();
        span.merge(then_block.span);
        var else_block: ?*Ast = null;
        if (self.lexer.consume_if_eq(&[_]types.Tag{.keyword_else})) |_| {
            else_block = try self.optional_block();
            span.merge(else_block.?.span);
        }
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

    if (self.lexer.consume_if_eq(&[_]types.Tag{.keyword_while})) |tok| {
        span.merge(tok.span);
        const condition = try self.expression();
        span.merge(condition.span);
        const body = try self.block();
        span.merge(body.span);
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
    span.merge(lval.span);
    if (self.lexer.consume_if_eq(&[_]types.Tag{
        .eq, .pluseq, .minuseq, .stareq, .slasheq, .percenteq, .shleq, .shreq, .ampeq, .careteq, .pipeeq
    })) |token| {
        span.merge(token.span);
        const expr = try self.expression();
        span.merge(expr.span);
        var parent: Ast = Ast.create(.{ .assignment = .{
            .op = token,
            .lvalue = lval,
            .expr = expr,
        }}, span);
        if (self.lexer.consume_if_eq(&[_]types.Tag{.semicolon})) |tok| {
            span.merge(tok.span);
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
    span.merge(condition.span);
    if (self.lexer.consume_if_eq(&[_]types.Tag{.question})) |_| {
        const true_path = try self.expression();
        span.merge(true_path.span);
        self.expect(.colon) catch |err| {
            span.start = true_path.span.start;
            try self.session.emit(.Error, span, "Expected ':'");
            return err;
        };
        const false_path = try self.ternary();
        span.merge(false_path.span);
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
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]types.Tag{.pipe2})) |pipe2| {
        const right = try self.logical_or();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = pipe2,
            .left = left,
            .right = right,
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
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]types.Tag{.amp2})) |amp2| {
        const right = try self.logical_and();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = amp2,
            .left = left,
            .right = right,
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
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]types.Tag{.pipe})) |pipe| {
        const right = try self.bitwise_or();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = pipe,
            .left = left,
            .right = right,
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
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]types.Tag{.caret})) |caret| {
        const right = try self.bitwise_xor();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = caret,
            .left = left,
            .right = right,
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
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]types.Tag{.amp})) |amp| {
        const right = try self.bitwise_and();
        span.merge(left.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = amp,
            .left = left,
            .right = right,
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
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]types.Tag{.eq2, .noteq})) |op| {
        const right = try self.equality();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = right,
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
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]types.Tag{.lt, .lteq, .gt, .gteq})) |op| {
        const right = try self.relational();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = right,
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
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]types.Tag{.shl, .shr})) |op| {
        const right = try self.shift();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = right,
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
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]types.Tag{.plus, .minus})) |op| {
        const right = try self.additive();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = right,
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
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]types.Tag{.star, .slash, .percent})) |op| {
        const right = try self.multiplicative();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = right,
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
        const unry = try self.unary();
        span.merge(unry.span);
        const out: Ast = Ast.create(.{ .unary_expr = .{
            .op = op,
            .expr = unry,
        }}, span);
        return try mem.createWith(self.gpa, out);
    }
    return try self.fn_call();
}


fn fn_call(self: *@This()) !*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index
    };

    const left = try self.access();
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]types.Tag{.open_paren})) |_| {
        var args = std.ArrayList(*Ast).init(self.gpa);
        while (!self.lexer.is_next_token(.close_paren)) {
            const expr = try self.expression();
            try args.append(expr);
            span.merge(expr.span);
            if (!self.lexer.is_next_token(.close_paren)) {
                try self.expect(.comma);
            }
        }
        try self.expect(.close_paren);
        const out: Ast = Ast.create(.{ .fn_call = .{
            .func = left,
            .args = try args.toOwnedSlice()
        }}, span);
        return try mem.createWith(self.gpa, out);
    }
    return left;
}

fn access(self: *@This()) !*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index
    };
    const left = try self.struct_cons();
    if (self.lexer.consume_if_eq(&[_]types.Tag{.dot})) |_| {
        span.merge(left.span);
        const ident = try self.expect_ret(.ident);
        span.merge(.{ .start = span.start, .end = self.lexer.index});
        const out: AstTypes.Ast = Ast.create(.{.access_operator = .{
            .left = left,
            .right = .{ .span = ident.span, .value = ident.span.get_string(self.lexer.buffer)}
        }}, span);
        return try mem.createWith(self.gpa, out);
    }
    return left;
}

fn struct_cons(self: *@This()) !*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index
    };
    const saved = self.lexer.index;
    const ty = self.parse_type() catch null ;
    if (self.lexer.consume_if_eq(&[_]types.Tag{.open_bracket})) |_| {
        const tyid = ty.?.hash();
        _ = try self.type_map.getOrPutValue(tyid, ty.?);
        var fields = std.StringHashMap(*Ast).init(self.gpa);
        while (!self.lexer.is_next_token(.close_bracket)) {
            try self.expect(.dot);
            const field = try self.expect_ret(.ident);
            const field_name = field.span.get_string(self.lexer.buffer);
            try self.expect(.eq);
            const value = try self.expression();
            if (!self.lexer.is_next_token(.comma) 
                and !self.lexer.is_next_token(.close_bracket)) {
                span.merge(.{.start = span.start, .end = self.lexer.index});
                try self.session.emit(.Error, span, "Missing comma before struct initializer");
                return error.MissingComma;
            }
            _ = self.lexer.consume_if_eq(&[_]types.Tag{.comma});
            if (fields.contains(field_name)) {
                span.merge(.{.start = span.start, .end = self.lexer.index});
                try self.session.emit(.Error, span, "Duplicate field initialization");
                return error.DuplicateFieldInit;
            }
            try fields.put(field_name, value);
        }
        try self.expect(.close_bracket);
        span.merge(.{.start = span.start, .end = self.lexer.index});
        const out = Ast.create(.{
            .struct_cons = .{
                .ty = tyid,
                .fields = fields
            }
        }, span);
        return try mem.createWith(self.gpa, out);
    }
    self.lexer.index = saved;
    return try self.primary();
}

fn primary(self: *@This()) anyerror!*Ast {
    var span: types.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    if (self.lexer.consume_if_eq(&[_]types.Tag{.ident, .float_literal, .int_literal, .string_literal, .raw_string_literal, .char_literal, .keyword_true, .keyword_false})) |lit| {
        span.end = self.lexer.index;
        const out: Ast = Ast.create(.{ .terminal = lit }, lit.span);
        return try mem.createWith(self.gpa, out);
    }
    if (self.lexer.consume_if_eq(&[_]types.Tag{.open_paren})) |tok| { 
        if (self.lexer.consume_if_eq(&[_]types.Tag{.close_paren})) |ctok| {
            span.merge(ctok.span);
            const out: Ast = Ast.create(.unit, span);
            return try mem.createWith(self.gpa, out);
        }
        span.start = tok.span.start;
        span.end = self.lexer.index;
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
    span.merge(self.lexer.next_token().span);
    try self.session.emit(.Error, span, "Unexpected Token");
    return error.Invalid;
}




