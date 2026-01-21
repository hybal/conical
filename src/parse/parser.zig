//! This file contains the parser for the conical language
const std = @import("std");

const Ast = @import("Ast.zig");
const AstNode = @import("Ast.zig").AstNode;
const Block = @import("Ast.zig").Block;
const lex = @import("lex");
const common = @import("common");
const diag = @import("diagnostics");
const errors = @import("errors.zig");

gpa: std.mem.Allocator,
lexer: lex.Lexer,
context: *common.Context,
has_module: bool = false,
builder: Ast.AstBuilder,



/// Initialize the parser from an already existing Lexer instance
pub fn init_from_lexer(in: lex.Lexer, context: *common.Context, gpa: std.mem.Allocator) @This() {
    return .{
        .lexer = in,
        .gpa = gpa,
        .context = context,
        .imports = .init(gpa),
        .builder = .init(gpa),
    };
}

/// Initialize the parser from source
pub fn init(context: *common.Context, gpa: std.mem.Allocator) @This() {
    return .{
        .lexer = lex.Lexer.init(context.source),
        .gpa = gpa,
        .imports = .init(gpa),
        .context = context,
        .builder= .init(gpa),
    };
}

/// The entrypoint for the parser
pub fn parse(self: *@This()) !Ast {
    while (self.lexer.has_next()) {
    }
}


// expect a token type,  does not return anything
fn expect(self: *@This(), token: lex.Tag) !void {
    if (self.lexer.consume_if_eq(&[_]lex.Tag{token})) |_| {
        return;
    } else {
        return error.UnexpectedToken;
    }
}

// expect a token type and return it if it is valid
fn expect_ret(self: *@This(), token: lex.Tag) !lex.Token {
    if (self.lexer.consume_if_eq(&[_]lex.Tag{token})) |tok| {
        return tok;
    } else {
        try self.context.session.emit(.Error, self.lexer.next_token().span, "Unexepected token ret");
        return error.UnexpectedToken;
    }
}

//this is the first thing to be ran
fn stmt(self: *@This()) anyerror!Ast.AstNodeId {
    return self.module_decl();
}

fn expression(self: *@This()) anyerror!Ast.AstNodeId {
    return self.return_stmt();
}


// a required block 
// block = "{" stmt* "}"
fn block(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index
    };
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.open_bracket})) |_| {
        span.start = self.lexer.index;
        var exprs = std.ArrayList(Ast.AstNodeId).init(self.gpa);
        while (self.lexer.has_next() and !self.lexer.is_next_token(.close_bracket)) {
            const exp = try self.stmt();
            span.merge(exp.span);
            if (self.lexer.consume_if_eq(&[_]lex.Tag{.semicolon})) |_| {
                const termnt: Ast = Ast.create(.{ .terminated = exp }, span);
                span.end = self.lexer.index;
                try exprs.append(try common.createWith(self.gpa, termnt)); 
            } else {
                try exprs.append(exp);
            }
        } else if (!self.lexer.has_next()) {
            try self.context.session.emit(.Error, span, "Unclosed curly-bracket");
            return error.UnmatchedBracket;
        }
        _ = self.lexer.next_token();
        span.end = self.lexer.index;
        const out: Ast = Ast.create(.{ .block = .{ .exprs = try exprs.toOwnedSlice() }}, span);
        return try common.createWith(self.gpa, out);
    } 
    try self.context.session.emit(.Error, span, "Expected a block");
    return error.RequiredBlock;
}

// an optional block
// e.g. 
// let x = { 
//      let b = 45;
//      let c = 32;
//      b + c
// }; 
// or,
// let x = 45 + 32;
// opt_block = block | expression
fn optional_block(self: *@This()) !Ast.AstNodeId {
    if (self.lexer.is_next_token(.open_bracket)) {
        return self.block();
    }
    return self.expression();
}

// parse a type with modifiers and primitives
// type = typemods* (primitivetype | ident)
fn parse_type(self: *@This()) !Ast.Type { 
    self.lexer.skip_whitespace();
    var modifiers = std.ArrayList(Ast.TypeModifier).init(self.gpa);
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index
    };
    while (self.lexer.consume_if_eq(&[_]lex.Tag{.amp, .amp2, .star, .open_square, .keyword_mut, .keyword_const})) |mmod| {
                
        var mod: Ast.TypeModifier = .Ref;
        switch (mmod.tag) {
            .amp => mod = .Ref, //&
            .amp2 => { //double ref since its a seperate token
                mod = .Ref;
                try modifiers.append(mod);
            },
            .star => mod = .Ptr, //*
            .keyword_mut => { //&mut or *mut
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
            .keyword_const => { //this may get removed
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
            .open_square => { //slice ([]) or static array ([x])
                if (self.lexer.consume_if_eq(&[_]lex.Tag{.int_literal})) |lit| {
                    mod = .{ .Array = lit };
                } else {
                    mod = .Slice;
                }
                self.expect(.close_square) catch |err| {
                    try self.context.session.emit(.Error, span, "Expected a closing square bracket '['");
                    return err;
                };
            },
            else => unreachable
        }
        try modifiers.append(mod);
    }
    var base_ty: Ast.Type = .createPrimitive(.Unit, if (modifiers.items.len > 0) try modifiers.toOwnedSlice() else null );
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.ident, .open_paren})) |ty| {
        if (ty.tag == .open_paren) {
            if (self.lexer.consume_if_eq(&[_]lex.Tag{.close_paren})) |_| { //()
                if (base_ty.modifiers != null) {
                    span.end = self.lexer.index;
                    try self.context.session.emit(.Error, span, "Unit type cannot have modifiers");
                    return error.UnitCannotHaveMods;
                }
            } else {
                //this is eventually where generics will be parsed (e.g. Type($T:i32))
                try self.context.session.emit(.Error, span, "Types cannot currently have paranthesis other than unit");
                return error.ParenInTypeExprNotUnit;
            }
        } else if (Ast.PrimitiveType.prims.get(ty.span.get_string(self.lexer.buffer))) |val| {
            base_ty.base_type = .{ .primitive = val };
        }  else {
            base_ty.base_type = .{ .user = .{ .span = ty.span, .value = ty.span.get_string(self.lexer.buffer) } };
        }
    } else if (self.lexer.consume_if_eq(&[_]lex.Tag{.keyword_struct})) |_|{
        try self.expect(.open_bracket);
        var fields = std.StringHashMap(Ast.TypeId).init(self.gpa);

        while (!self.lexer.is_next_token(.close_bracket)) {
            const ident = try self.expect_ret(.ident);
            try self.expect(.colon);
            const field_type = try self.parse_type();
            if (!self.lexer.is_next_token(.comma) and !self.lexer.is_next_token(.close_bracket)) {
                span.merge(.{ .start = span.start, .end = self.lexer.index});
                try self.context.session.emit(.Error, span, "Missing comma");
                return error.MissingCommaInStruct;
            }
            _ = self.lexer.consume_if_eq(&[_]lex.Tag{.comma});
            fields.putNoClobber(ident.span.get_string(self.lexer.buffer), field_type.hash()) catch {
                span.merge(.{ .start = span.start, .end = self.lexer.index});
                try self.context.session.emit(.Error, span, "Struct with duplicate fields");
                return error.DuplicateFields;
            };
        }
        try self.expect(.close_bracket);
        base_ty.base_type = .{ .strct = .{ .fields = fields } };
    } else if (self.lexer.consume_if_eq(&[_]lex.Tag{.keyword_enum})) |_| {
        try self.expect(.open_bracket);
        var variants = std.StringHashMap(?Ast.TypeId).init(self.gpa);

        while (!self.lexer.is_next_token(.close_bracket)) {
            const ident = try self.expect_ret(.ident);
            var var_ty: ?Ast.TypeId = null;
            if (self.lexer.consume_if_eq(&[_]lex.Tag{.colon})) |_| {
                var_ty = (try self.parse_type()).hash();
            }
            if (!self.lexer.is_next_token(.comma) and !self.lexer.is_next_token(.close_bracket)) {
                span.merge(.{ .start = span.start, .end = self.lexer.index});
                try self.context.session.emit(.Error, span, "Missing comma");
                return error.MissingCommaInEnum;
            }
            _ = self.lexer.consume_if_eq(&[_]lex.Tag{.comma});
            variants.putNoClobber(ident.span.get_string(self.lexer.buffer), var_ty) catch {
                span.merge(.{ .start = span.start, .end = self.lexer.index});
                try self.context.session.emit(.Error, span, "Duplicate enum variant");
                return error.DuplicateEnumVariant;
            };
        }
        try self.expect(.close_bracket);
        base_ty.base_type = .{ .@"enum" = .{ .variants = variants } };
    }
    const tyid = base_ty.hash();
    _ = try self.context.type_tab.getOrPutValue(tyid, base_ty);
    base_ty.chash = tyid;
    return base_ty;
}

// trys to parse a global decleration modifier otherwise returns null
fn try_decl_mod(self: *@This()) !?Ast.GlobalDeclMod {
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.keyword_pub, .keyword_export, .keyword_extern})) |key| {
        var out: Ast.GlobalDeclMod = .Pub;
        switch (key.tag) {
            .keyword_pub => {
                if (self.lexer.consume_if_eq(&[_]lex.Tag{.keyword_export})) |_| {
                    out = .PubExport;
                }
                if (self.lexer.consume_if_eq(&[_]lex.Tag{.keyword_extern})) |_| {
                    out = .PubExtern;
                }
                out = .Pub;
            },
            .keyword_export => out = .Export,
            .keyword_extern => out = .Extern,
            else => unreachable,
        }
        if (try self.try_decl_mod()) |_| {
            try self.context.session.emit(.Error, key.span, "Invalid decleration modifier");
            return error.InvalidGlobalMod;
        }
        return out;
    }
    return null;
}



// parses a function decleration
// NOTE: This may be changed to the more common inline style
// fn_decl = decl_mod? fn_mod? "fn" ident "(" param* ")" (":" "(" (type ",") | type ")" )? ("->" type)? block?
fn fn_decl(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var decl_mod: ?Ast.GlobalDeclMod = null;
    var fn_mod: ?Ast.FnModifier = null;
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };
    if (try self.try_decl_mod()) |decl| {
        span.merge(.{ .start = span.start, .end = self.lexer.index });
        decl_mod = decl;
    }
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.keyword_async, .keyword_pure, .keyword_comp})) |fnm| {
        fn_mod = switch (fnm.tag) {
            .keyword_async => .Async,
            .keyword_pure => .Pure,
            .keyword_comp => .CompTime,
            else => unreachable
        };
    }
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.keyword_fn})) |_| {
        var params = std.ArrayList(Ast.Ident).init(self.gpa);
        const ident = try self.expect_ret(.ident);
        span.merge(ident.span);
        self.expect(.open_paren) catch |err| {
            span.merge(.{.start = span.start, .end = self.lexer.index});
            try self.context.session.emit(.Error, span, "Expected an open parenthesis");
            return err;
        };
        while (!self.lexer.is_next_token(.close_paren)) {
            const tok = self.lexer.next_token();
            span.merge(tok.span);
            if (tok.tag == .ident) {
                try params.append(.{ .span = tok.span, .value = tok.span.get_string(self.lexer.buffer)});
            } else if (tok.tag == .comma) {
                if (self.lexer.is_next_token(.close_paren)) {
                    try self.context.session.emit(.Error, span, "Parameter list contains an extra comma");
                    return error.UnfinishedParameterList;
                }
            } else {
                try self.context.session.emit(.Error, span, "Function signature contains an invalid token");
                return error.InvalidTokenInFunctionSignature;
            }
        }
        _ = self.lexer.next_token();
        var param_types = std.ArrayList(Ast.TypeId).init(self.gpa);
        if (self.lexer.consume_if_eq(&[_]lex.Tag{.colon})) |_| {
            if (self.lexer.consume_if_eq(&[_]lex.Tag{.open_paren})) |_| {
                while (!self.lexer.is_next_token(.close_paren)) {
                    const param_ty = try self.parse_type();
                    const param_tyid = param_ty.hash();
                    _ = try self.context.type_tab.getOrPutValue(param_tyid, param_ty);
                    try param_types.append(param_tyid);
                    if (self.lexer.consume_if_eq(&[_]lex.Tag{.comma})) |_| {
                        if (self.lexer.is_next_token(.close_paren)) {
                            span.merge(.{.start = span.start, .end = self.lexer.index});
                            try self.context.session.emit(.Error, span, "Type parameter list contains an extra comma");
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
            try self.context.session.emit(.Error, span, try std.fmt.allocPrint(self.gpa, "Type list is shorter than the parameter list. Expected: {}, got: {}", .{params.items.len, param_types.items.len}));
            return error.MismatchedParamToTypeLen;
        }
        var return_ty: Ast.Type = Ast.Type.createPrimitive(.Unit, null);
        if (self.lexer.consume_if_eq(&[_]lex.Tag{.thin_arrow})) |_| {
            return_ty = try self.parse_type();
            span.merge(.{.start = span.start, .end = self.lexer.index});
        }
        var blck: ?Ast.AstNodeId = null;
        if (!self.lexer.is_next_token(.semicolon)) {
            blck = try self.block();
            span.merge(blck.?.span);
        } else {
            _ = self.lexer.next_token();
        }
        const retid = return_ty.hash();
        _ = try self.context.type_tab.getOrPutValue(retid, return_ty);
        const out: Ast = Ast.create(.{ .fn_decl = .{
            .ident = .{ .span = ident.span, .value = ident.span.get_string(self.lexer.buffer)},
            .params = try params.toOwnedSlice(),
            .param_types = try param_types.toOwnedSlice(),
            .return_ty = retid,
            .decl_mod = decl_mod,
            .fn_mod = fn_mod,
            .body = blck
        }}, span);
        return try common.createWith(self.gpa, out);
    }
    return self.var_decl();
}

// parses a variable decleration
// var_decl = ("let" | "mut") (":" type)? ("=" expression)? ";"
fn var_decl(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    if (self.lexer.consume_if_eq(&[_]lex.Tag{.keyword_let, .keyword_mut})) |key| {
        const ident = try self.expect_ret(.ident);
        span.merge(key.span);
        span.merge(ident.span);
        var ty: ?Ast.Type = null;
        if (self.lexer.consume_if_eq(&[_]lex.Tag{.colon})) |_| {
            ty = try self.parse_type();
            span.merge(.{.start = span.start, .end = self.lexer.index});
        }
        var initial: ?Ast.AstNodeId = null;
        if (self.lexer.consume_if_eq(&[_]lex.Tag{.eq})) |eq| {
            span.merge(eq.span);
            initial = try self.expression();
            span.merge(initial.?.span);
        }
        var tyid: ?u64 = null;
        if (ty) |tyy| {
            tyid = tyy.hash();
            _ = try self.context.type_tab.getOrPutValue(tyid.?, tyy);
        }
        var out: Ast = Ast.create(.{ .var_decl = .{
            .is_mut = key.tag == .keyword_mut,
            .ident = .{ .span = ident.span, .value = ident.span.get_string(self.lexer.buffer)},
            .ty = tyid,
            .initialize = initial
        }}, span);
        if (self.lexer.consume_if_eq(&[_]lex.Tag{.semicolon})) |tok| {
            span.merge(tok.span);
            out = Ast.create(.{ .terminated = try common.createWith(self.gpa, out)}, span);
        } else {
            try self.context.session.emit(.Error, span, "Missing Semicolon");
            return error.MissingSemicolon;
        }
        return try common.createWith(self.gpa, out);
    }
    return try self.type_decl();
}

fn type_decl(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    if (self.lexer.consume_if_eq(&[_]lex.Tag{.keyword_type})) |_| {
        const ident = try self.expect_ret(.ident);
        try self.expect(.eq);
        const ty = try self.parse_type();
        try self.expect(.semicolon);
        const tyid = ty.hash();
        span.end = self.lexer.index;
        _ = try self.context.type_tab.getOrPutValue(tyid, ty);
        const out: Ast = Ast.create(.{ .type_decl = .{
            .ident = .{ 
                .span = ident.span,
                .value = ident.span.get_string(self.lexer.buffer)
            },
            .ty = tyid,
        }}, span);
        return try common.createWith(self.gpa, out);
    }
    return self.expression();

}

fn module_decl(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.keyword_mod})) |key| {
        if (self.has_module) {
            span.merge(.{ .start = span.start, .end = self.lexer.index});
            try self.context.session.emit(.Error, span, "There can only be one module per file");
            return error.ModuleError;
        }
        span.merge(key.span);
        const path = try self.parse_path();
        if (path == null) {
            span.merge(.{.start= span.start, .end = self.lexer.index});
            try self.context.session.emit(.Error, span, "Malformed module path");
            return error.MalformedModulePath;
        }
        try self.expect(.semicolon);
        const mod = common.Module {
            .exports = .init(self.gpa),
            .imports = .init(self.gpa),
            .path = path.?.node.path,
            .source_file = self.context.file_path,
        };
        const modid = mod.hash();
        try self.context.module_store.put(modid, mod);
        self.context.module = modid;
        self.has_module = true;
    }
    if (self.has_module) {
        return self.import_stmt();
    }
    span.merge(.{.start= span.start, .end = self.lexer.index});
    try self.context.session.emit(.Error, span, "Expected Module Decleration");
    return error.ExpectedModule;
}

fn import_stmt(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    if (self.lexer.consume_if_eq(&[_]lex.Tag{.keyword_import})) |key| {
        span.merge(key.span);
        const path = try self.parse_path();
        if (path == null) {
            span.merge(.{.start= span.start, .end = self.lexer.index});
            try self.context.session.emit(.Error, span, "Malformed import path");
            return error.MalformedModulePath;
        }
        const node: Ast.Import = .{
            .path = path.?,
        };
        const out = Ast.create(.{ .import = node }, span);
        try self.imports.append(path.?.node.path);
        return try common.createWith(self.gpa, out);
    }
    return self.fn_decl();
}

fn return_stmt(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.keyword_return})) |tok| {
        span.merge(tok.span);
        const expr = try self.expression();
        span.merge(expr.span);
        const out = Ast.create(.{ .return_stmt = expr }, span);
        return try common.createWith(self.gpa, out);
    }
    return self.ifstmt();
}
fn ifstmt(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.keyword_if})) |tok| {
        span.merge(tok.span);
        const condition = try self.expression();
        span.merge(condition.span);
        const then_block = try self.optional_block();
        span.merge(then_block.span);
        var else_block: ?Ast.AstNodeId = null;
        if (self.lexer.consume_if_eq(&[_]lex.Tag{.keyword_else})) |_| {
            else_block = try self.optional_block();
            span.merge(else_block.?.span);
        }
        const out: Ast = Ast.create(.{ .if_stmt = .{
            .condition = condition,
            .block = then_block,
            .else_block = else_block
        }}, span);
        return try common.createWith(self.gpa, out);
    }
    return try self.while_loop();
}

//TODO: move this (wrong precedence)
fn while_loop(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    if (self.lexer.consume_if_eq(&[_]lex.Tag{.keyword_while})) |tok| {
        span.merge(tok.span);
        const condition = try self.expression();
        span.merge(condition.span);
        const body = try self.block();
        span.merge(body.span);
        const out: Ast = Ast.create(.{ .while_loop = .{
            .condition = condition,
            .block = body
        }}, span);
        return try common.createWith(self.gpa, out);
    }
    return try self.assignment();
}

//TODO: move this (wrong precedence)
fn assignment(self: *@This()) anyerror!Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    const lval = try self.ternary();
    span.merge(lval.span);
    if (self.lexer.consume_if_eq(&[_]lex.Tag{
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
        
        if (token.tag != .eq) {
            const operator: lex.Tag = switch (token.tag) {
                    .pluseq => .plus,
                    .minuseq => .minus,
                    .stareq => .star,
                    .slasheq => .slash,
                    .percenteq => .percent,
                    .shleq => .shl,
                    .shreq => .shr,
                    .ampeq => .amp,
                    .careteq => .caret,
                    .pipeeq => .pipe,
                    else => unreachable,
            };
            const binary_expr: Ast = Ast.create(.{ .binary_expr = .{
                .left = lval,
                .right = expr,
                .op = .{ .tag = operator, .span = token.span},
            }}, span);
            parent = Ast.create(.{ .assignment = .{
                .op = .{ .tag = .eq, .span = token.span },
                .lvalue = lval,
                .expr = try common.createWith(self.gpa, binary_expr),
            }}, span);
        }
        
        if (self.lexer.consume_if_eq(&[_]lex.Tag{.semicolon})) |tok| {
            span.merge(tok.span);
            parent = Ast.create(.{ .terminated = try common.createWith(self.gpa, parent) }, span);
        }
        return try common.createWith(self.gpa, parent);
    } 
    return lval;
}

fn ternary(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var condition = try self.logical_or();
    span.merge(condition.span);
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.question})) |_| {
        const true_path = try self.expression();
        span.merge(true_path.span);
        self.expect(.colon) catch |err| {
            span.start = true_path.span.start;
            try self.context.session.emit(.Error, span, "Expected ':'");
            return err;
        };
        const false_path = try self.ternary();
        span.merge(false_path.span);
        const parent: Ast = Ast.create(.{ .ternary = .{
            .condition = condition,
            .true_path = true_path,
            .false_path = false_path,
        }}, span);
        condition = try common.createWith(self.gpa, parent);
    }
    return condition;
}

fn logical_or(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.logical_and();
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.pipe2})) |pipe2| {
        const right = try self.logical_or();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = pipe2,
            .left = left,
            .right = right,
        }}, span);
        left = try common.createWith(self.gpa, parent);
    }
    return left;
}
fn logical_and(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.bitwise_or();
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.amp2})) |amp2| {
        const right = try self.logical_and();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = amp2,
            .left = left,
            .right = right,
        }}, span);
        left = try common.createWith(self.gpa, parent);
    }
    return left;
}

fn bitwise_or(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.bitwise_xor();
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.pipe})) |pipe| {
        const right = try self.bitwise_or();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = pipe,
            .left = left,
            .right = right,
        }}, span);
        left = try common.createWith(self.gpa, parent);
    }
    return left;
}
fn bitwise_xor(self: *@This()) anyerror!Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.bitwise_and();
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.caret})) |caret| {
        const right = try self.bitwise_xor();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = caret,
            .left = left,
            .right = right,
        }}, span);
        left = try common.createWith(self.gpa, parent);
    }
    return left;
}
fn bitwise_and(self: *@This()) anyerror!Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.equality();
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.amp})) |amp| {
        const right = try self.bitwise_and();
        span.merge(left.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = amp,
            .left = left,
            .right = right,
        }}, span);
        left = try common.createWith(self.gpa, parent);
    }
    return left;
}

fn equality(self: *@This()) anyerror!Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.relational();
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.eq2, .noteq})) |op| {
        const right = try self.equality();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = right,
        }}, span);
        left = try common.createWith(self.gpa, parent);
    }
    return left;
}

fn relational(self: *@This()) anyerror!Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.shift();
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.lt, .lteq, .gt, .gteq})) |op| {
        const right = try self.relational();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = right,
        }}, span);
        left = try common.createWith(self.gpa, parent);
    }
    return left;
}
fn shift(self: *@This()) anyerror!Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.additive();
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.shl, .shr})) |op| {
        const right = try self.shift();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = right,
        }}, span);
        left = try common.createWith(self.gpa, parent);
    }
    return left;
}

fn additive(self: *@This()) anyerror!Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.multiplicative();
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.plus, .minus})) |op| {
        const right = try self.additive();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = right,
        }}, span);
        left = try common.createWith(self.gpa, parent);
    }
    return left;
}
fn multiplicative(self: *@This()) anyerror!Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    var left = try self.cast();
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.star, .slash, .percent})) |op| {
        const right = try self.multiplicative();
        span.merge(right.span);
        const parent: Ast = Ast.create(.{ .binary_expr = .{
            .op = op,
            .left = left,
            .right = right,
        }}, span);
        left = try common.createWith(self.gpa, parent);
    }
    return left;
}
fn cast(self: *@This()) anyerror!Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index
    };
    const expr = try self.unary();
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.keyword_as})) |_| {
        const ty = try self.parse_type();
        span.merge(.{.start = span.start, .end = self.lexer.index});
        const ty_hash = ty.hash();
        _ = try self.context.type_tab.getOrPutValue(ty_hash, ty);
        const out: Ast = Ast.create(.{ .cast = .{
            .expr = expr,
            .ty = ty_hash,
        }}, span);
        return try common.createWith(self.gpa, out);
    }
    return expr;
}

fn unary(self: *@This()) anyerror!Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index,
    };

    if (self.lexer.consume_if_eq(&[_]lex.Tag{.minus, .bang, .tilde, .star, .amp, .amp2})) |op| {
        const unry = try self.unary();
        span.merge(unry.span);
        const out: Ast = Ast.create(.{ .unary_expr = .{
            .op = op,
            .expr = unry,
        }}, span);
        return try common.createWith(self.gpa, out);
    }
    return try self.fn_call();
}



fn fn_call(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index
    };

    const left = try self.access();
    span.merge(left.span);
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.open_paren})) |_| {
        var args = std.ArrayList(Ast.AstNodeId).init(self.gpa);
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
            .left = left,
            .params = try args.toOwnedSlice()
        }}, span);
        return try common.createWith(self.gpa, out);
    }
    return left;
}

fn access(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index
    };
    const left = try self.type_cons();
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.dot})) |_| {
        span.merge(left.span);
        const ident = try self.expect_ret(.ident);
        span.merge(ident.span);
        const out: Ast.Ast = Ast.create(.{.access_operator = .{
            .left = left,
            .right = .{ .span = ident.span, .value = ident.span.get_string(self.lexer.buffer)}
        }}, span);
        return try common.createWith(self.gpa, out);
    }
    return left;
}

fn initializer(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index + 1,
    };
    
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.dot})) |dot_token| {
        span.merge(dot_token.span);
        var typ: ?Ast.TypeExpr = null;
        if (self.lexer.consume_if_eq(&[_]lex.Tag{.open_paren})) |open_paren_token| {
            span.merge(open_paren_token.span);
            const ty = try self.parse_type();
            self.expect(.close_paren) catch {
                span.merge(.{.start = span.start, .end = self.lexer.index});
                const err = errors.UnmatchedDelimeterError {
                    .span = span,
                    .expected_delim = .close_paren,
                    .start_delim = open_paren_token.span,
                };
                const errid = try self.context.session.push(err.get_error_type());
                const out = try self.builder.add_node(
                    .poison,
                    span,
                    Ast.Poison { .error_id = errid },
                );
                return out;
            };
            typ = ty;
        }
        if (!self.lexer.is_next_token(.open_bracket)) {
            const tok = self.lexer.next_token();
            const err = errors.MalformedInitializerError {
                .kind = .missing_bracket,
                .previous_field = null,
                .span = tok.span,
            };
            const errid = try self.context.session.push(err.get_error_type());
            const out = try self.builder.add_node(
                .poison,
                span,
                Ast.Poison { .error_id = errid },
            );
            return out;
        }
        self.expect(.open_bracket) catch unreachable;
        if (self.lexer.has_next() and !self.lexer.is_next_token(.close_bracket)) {
            if (self.lexer.is_next_token(.ident)) {
                const id = self.lexer.next_token();
                span.merge(id.span);
                if (self.lexer.is_next_token(.colon)) {
                    var fields = std.ArrayList(Ast.InitializerField).init(self.gpa);
                    const colon_token = self.lexer.next_token();
                    span.merge(colon_token.span);
                    const expr = try self.expression();
                    try fields.append(.{ .id = id, .value = expr });
                    while (self.lexer.consume_if_eq(&[_]lex.Tag{.comma})) |comma_token| {
                        span.merge(comma_token.span);
                        if (self.lexer.is_next_token(.close_bracket)) {
                            break;
                        }
                        if (self.lexer.is_next_token(.ident)) {
                            const id2 = self.lexer.next_token();
                            if (self.lexer.is_next_token(.colon)) {
                                _ = self.lexer.next_token();
                                const expr2 = try self.expression();
                                span.merge(.{ .start = span.start, .end = self.lexer.index });
                                try fields.append(.{ .id = id2, .value = expr2 });
                            } else {
                                //Expected colon error
                            }
                        } else {
                            //Expected identifier error
                        }
                    }
                    if (!self.lexer.is_next_token(.close_bracket)) {
                        //Unmatched delimeter error
                    }
                }
            }
        }
    }
}

fn type_cons(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index
    };
    const saved = self.lexer.index;
    self.context.session.freeze();
    const ty = self.parse_type() catch null;
    self.context.session.unfreeze();
    if (ty != null and self.lexer.is_next_token(.open_bracket)) {
        const tok = self.lexer.next_token();
        span.merge(.{.start = span.start, .end = self.lexer.index});
        var fields = std.StringHashMap(?Ast.AstNodeId).init(self.gpa);
        while (!self.lexer.is_next_token(.close_bracket)) {
            try self.expect(.dot);
            const field = try self.expect_ret(.ident);
            const field_name = field.span.get_string(self.lexer.buffer);
            const value = if (self.lexer.consume_if_eq(&[_]lex.Tag{.eq})) |_| try self.expression() else null; 
            span.merge(.{.start = span.start, .end = self.lexer.index});
            if (!self.lexer.is_next_token(.comma) 
                and !self.lexer.is_next_token(.close_bracket)) {
                span.merge(.{.start = span.start, .end = self.lexer.index});
                try self.context.session.emit(.Error, span, "Missing comma before struct initializer");
                return error.MissingComma;
            }
            _ = self.lexer.consume_if_eq(&[_]lex.Tag{.comma});
            if (fields.contains(field_name)) {
                span.merge(.{.start = span.start, .end = self.lexer.index});
                try self.context.session.emit(.Error, span, "Duplicate field initialization");
                return error.DuplicateFieldInit;
            }
            span.merge(.{.start = span.start, .end = self.lexer.index});
            try fields.put(field_name, value);
        }
        self.expect(.close_bracket) catch {
            span.merge(.{ .start = span.start, .end = self.lexer.index });
            const err = errors.UnmatchedDelimeterError {
                .expected_delim = .close_bracket,
                .start_delim = tok,
                .span = span,
            };
            const errid = try self.context.session.push(err.get_error_type());
            const out = try self.builder.add_node(
                .poison,
                span,
                Ast.Poison { .error_id = errid },
            );
            return out;
        };

        span.merge(.{.start = span.start, .end = self.lexer.index});
        const out = try self.builder.add_node(
            .type_cons,
            span,
            Ast.TypeCons {
                .ty = ty.?.hash(),
                .fields = fields,
            }
        );
        return out;
    }
    self.lexer.index = saved;
    return try self.primary();
}

fn parse_path(self: *@This()) !Ast.AstNodeId {
    try self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index + 1,
    };
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.ident})) |fid| {
        span.merge(fid.span);
        var parts = std.ArrayList(Ast.Ident).init(self.gpa);
        try parts.append(.{ .span = fid.span, .value = fid.span.get_string(self.lexer.buffer)});
        while (self.lexer.consume_if_eq(&[_]lex.Tag{.colon2})) |tok| {
            span.merge(tok.span);
            if (self.lexer.consume_if_eq(&[_]lex.Tag{.ident})) |id| {
                span.merge(id.span);
                try parts.append(.{ .span = id.span, .value = id.span.get_string(self.lexer.buffer)});
            } else if (self.lexer.consume_if_eq(&[_]lex.Tag{.colon2})) |consecutive| {
                const err = errors.MalformedPathError {
                    .kind = .consecutive,
                    .span = span,
                    .malformed_span = consecutive,
                };
                const errid = try self.context.session.push(err.get_error_type());
                const err_node = try self.builder.add_node(
                    .poison,
                    span,
                    Ast.Poison { .error_id = errid },
                );
                //TODO: collect more than just one error
                if (self.lexer.consume_if_eq(&[_]lex.Tag{.ident})) |_| {
                    while (self.lexer.consume_if_eq(&[_]lex.Tag{.colon2, .ident})) |_| {
                        
                    }
                }
                return err_node;
            } else {
                const err = errors.MalformedPathError {
                    .kind = .trailing,
                    .span = span,
                    .malformed_span = tok.span,
                };
                const errid = try self.context.session.push(err.get_error_type());
                const err_node = try self.builder.add_node(
                    .poison,
                    span,
                    Ast.Poison { .error_id = errid }
                );
                return err_node;
            }
        }
        const parts_slice = try parts.toOwnedSlice();
        const out = try self.builder.add_node(
            .path,
            span,
            Ast.Path { .parts = parts_slice },
        );
        return out;
    }
    // This function should always be called when the next token is an identifier
    // It is a compiler error if it isn't
    @compileError("Parser reached an unreachable state");
}


/// Parse a "primary" expression
/// This is mostly to be able to use the same terminal parser for both general and type expressions
/// As such this includes things like parenthesized expressions and inline blocks
fn primary(self: *@This()) !Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index + 1,
    };
    // Sub-expression parsing
    if (self.lexer.consume_if_eq(&[_]lex.Tag{.open_paren})) |tok| { 
        span.merge(tok.span);
        // If there is nothing in between the parenthesis it is interpreted as `unit` 
        // This may change in the future if I decide to make unit an actual empty set (something like `{ }` for type expressions and `.{ }` for general)
        if (self.lexer.consume_if_eq(&[_]lex.Tag{.close_paren})) |ctok| {
            span.merge(ctok.span);
            const out = try self.builder.add_node(
                .unit,
                span,
                Ast.Unit {}
            );
            return out;
        }
        const out = try self.expression();
        span.merge(out.span);
        self.expect(.close_paren) catch {
            span.merge(.{.start = span.start, .end = self.lexer.index});
            const err = errors.UnmatchedDelimeterError {
                .expected_delim = .close_paren,
                .start_delim = tok,
                .span = span
            };
            const errid = try self.context.session.push(err.get_error_type());
            const err_node = try self.builder.add_node(
                .poison,
                span,
                Ast.Poison { .error_id = errid },
            );
            return err_node;
        };
        return out;
    }
    if (self.lexer.is_next_token(.open_bracket)) {
        return try self.block();
    }
    return try self.terminal();

}

/// Parse a terminal node
/// This is stuff like literals and initializers, this does *not* include parenthesized expressions.
fn terminal(self: *@This()) anyerror!Ast.AstNodeId {
    self.lexer.skip_whitespace();
    var span: common.Span = .{
        .start = self.lexer.index,
        .end = self.lexer.index + 1,
    };

    if (self.lexer.is_next_token(.ident)) {
        return try self.parse_path();
    }
    if (self.lexer.consume_if_eq(
            &[_]lex.Tag{.float_literal, .int_literal, .string_literal, .raw_string_literal, .char_literal, .keyword_true, .keyword_false})) 
        |lit| {
        span.merge(lit.span);
        const out = try self.builder.add_node(
            .terminal,
            span,
            lit
        );
        return out;
    }
    span.merge(.{.start = span.start, .end = self.lexer.index});
    const err = errors.ExpectedExpressionError {
        .found = self.lexer.next_token(),
    };
    const errid = try self.context.session.push(err.get_error_type());
    const err_node = try self.builder.add_node(
        .poison,
        span,
        Ast.Poison {
            .error_id = errid,
        }
    );
    self.panic_recovery();
    return err_node;
}

//Crude recovery only, more context-aware recovery should be done inline
fn panic_recovery(self: *@This()) void {
    while (self.lexer.consume_if_eq(
            &[_]lex.Tag{.semicolon, .close_bracket, .close_paren, .close_square, .keyword_const, .keyword_let, .keyword_mut, .keyword_while, .keyword_for, .keyword_if, .keyword_else, .keyword_return, .keyword_break, .keyword_continue}) == null) {
        _ = self.lexer.next_token();
    }
}




