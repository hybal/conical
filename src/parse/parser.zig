//! This file contains the parser for the conical language


//NOTES:
//In the self-hosted rewrite the Ast will actually be a syntax tree so that span information is exact
const std = @import("std");

const Ast = @import("Ast.zig");
const AstNode = Ast.AstNode;
const AstNodeId = Ast.AstNodeId;
const Block = @import("Ast.zig").Block;
const lex = @import("lex");
const common = @import("common");
const diag = @import("diagnostics");
const errors = @import("errors.zig");

allocator: std.mem.Allocator,
lexer: lex.Lexer,
context: *common.Context,
has_module: bool = false,
builder: Ast.AstBuilder,
file: common.FileId,



/// Initialize the parser from an already existing Lexer instance
pub fn init_from_lexer(in: lex.Lexer, context: *common.Context, gpa: std.mem.Allocator) @This() {
    return .{
        .lexer = in,
        .allocator = gpa,
        .context = context,
        .builder = .init(gpa),
        .file = in.file,
    };
}

/// Initialize the parser from source
pub fn init(context: *common.Context, reader: *std.io.Reader, file: common.FileId, gpa: std.mem.Allocator) @This() {
    return .{
        .lexer = lex.Lexer.init(reader, file),
        .allocator = gpa,
        .context = context,
        .builder= .init(gpa),
        .file = file,
    };
}

pub fn deinit(self: *@This()) void {
    self.builder.deinit();
}


fn expect_ret(self: *@This(), tag: lex.Tag) !?lex.Token {
    if (!self.lexer.is_next_token(tag)) {
        return null;
    }
    return try self.lexer.next_token();
}

fn expect(self: *@This(), tag: lex.Tag) !bool {
    const out = try self.expect_ret(tag);
    if (out) return true;
    return false;
}

fn is_next_one_of(self: *@This(), comptime values: anytype) bool {
    const next_tok_option = self.peek();
    if (next_tok_option == null) return false;
    const next_tok = next_tok_option.?;
    inline for (values) |val| {
        if (next_tok.tag == val) return true;
    }
    return false;
}

fn is_next(self: *@This(), tag: lex.Tag) bool {
    const next_tok = self.peek();
    if (next_tok == null) return false;
    return next_tok.?.tag == tag;
}

fn next_if(self: *@This(), tag: lex.Tag) ?lex.Token {
    if (self.is_next(tag)) return self.next() catch unreachable;
    return null;
}

fn next(self: *@This()) !lex.Token {
    const out = self.lexer.next_token();
    if (out.tag == .eof) {
        return error.EOF;
    }
    return out;
}

fn peek(self: *@This()) ?lex.Token {
    const next_tok = self.lexer.peek_token();
    if (next_tok.tag == .eof) {
        return null;
    }
    return next_tok;
}


/// The entrypoint for the parser
pub fn parse(self: *@This()) !Ast {
    return try self.program();
}

fn program(self: *@This()) !Ast {
    var span: common.Span = .init(self.lexer.index, self.file);
    // Every source file requires a module declaration as the first thing in the file.
    const mod = try self.module_declaration();
    span.merge(self.builder.get_span(mod.path));
    var decls = std.ArrayList(AstNodeId).empty;
    while (self.lexer.has_next()) {
        const decl = try self.declaration();
        span.merge(self.builder.get_span(decl));
        try decls.append(self.allocator, decl);
    }
    const prog = Ast.Program {
        .declarations = try decls.toOwnedSlice(self.allocator),
        .module = mod,
    };

    self.builder.set_program(prog);
    defer self.deinit();
    return try self.builder.build();
}

fn module_declaration(self: *@This()) !?Ast.ModuleDecl {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (!self.is_next(.keyword_mod)) {
        //ERROR: expected module declaration
        return null;
    }
    span.merge(self.lexer.next_token().span);
    const path = try self.expression_path();
    if (self.builder.is_poison(path)) {
        //ERROR: malformed path in module declaration
        return null;
    }
    if(!try self.expect(.semicolon)) {
        //ERROR: Expected semicolon
    }
    return .{
        .path = path,
    };

}

fn item(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    const vis = try self.visibility();
    const link = try self.linkage();
    const function_mods = try self.function_modifiers();
    const peek_tok = self.peek();
    if (peek_tok == null) {
        return error.EOF;
    }
    const kind = switch (peek_tok.tag) {
        .keyword_fn => .{.function, try self.function_declaration()},
        .keyword_let => .{.binding, try self.let_binding()},
        .keyword_type => .{.@"type", try self.type_declaration()},
    };

    if (link) |l| span.merge(l.span);
    if (vis) |v| span.merge(v.span);
    if (function_mods) |ms| {
        for (ms) |i| {
            span.merge(i.span);
        }
    }

    span.merge(self.builder.get_span(kind));
    const node = Ast.Item {
        .item = kind,
        .item_kind = kind[0],
        .linkage = link,
        .visibility = vis,
        .function_mods = function_mods,
    };

    const nodeid = try self.builder.add_node(
        .item,
        span,
        node,
    );
    return nodeid;
        
}

fn visibility(self: *@This()) !?Ast.Visibility {
    if (self.next_if(.keyword_pub)) |v| {
        return .{
            .kind = .public,
            .span = v.span,
        };
    }
    return null;
}

fn linkage(self: *@This()) !?Ast.Linkage {
    if (self.next_if(.keyword_extern)) |tok| {
        return .{
            .kind = .@"extern",
            .span = tok.span
        };
    }
    if (self.next_if(.keyword_export)) |tok| {
        return .{
            .kind = .@"export",
            .span = tok.span,
        };
    }
    return null;
}

fn function_modifiers(self: *@This()) ![]Ast.FnMod {
    var mods: std.ArrayList(Ast.FnMod) = .empty;
    while (self.is_next_one_of(.{.keyword_inline, .keyword_pure, .keyword_comptime})) {
        const tok = try self.next();
        const kind: Ast.FnModKind = switch (tok.tag) {
            .keyword_inline => .@"inline",
            .keyword_pure => .pure,
            .keyword_comptime => .@"comptime",
        };
        try mods.append(self.allocator, .{ .kind = kind, .span = tok.span });
    }
    return try mods.toOwnedSlice();
}

fn let_binding(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    const let_keyword = try self.expect_ret(.keyword_let);
    if (let_keyword == null) {
        //FATAL: This should never be null, there is a problem in the compiler
    }
    
    const modifier = try self.binding_modifier();

    const ident = try self.expect_ret(.ident);

    if (ident == null) {
        //ERROR: Expected identifier before '='
    }
    var typeexpr: Ast.TypeExpr = null;
    if (self.next_if(.colon)) |tok| {
        span.merge(tok.span);
        typeexpr = try self.type_expression();
    }
    if (!try self.expect(.eq)) {
        //ERROR: Expected '='
    }
    const expr = try self.expression();
    span.merge(self.builder.get_span(expr));
    //FIXME: we somehow need access to either a reversible Reader or a fresh one or somehow recover source information.
    const node = Ast.VarDecl {
        .ident = .{ .span = ident.a.span },
        .initialize = expr,
        .ty = typeexpr,
        .modifier = modifier,
    };

    const nodeid = try self.builder.add_node(.var_decl, span, node);
    return nodeid;
}

fn binding_modifier(self: *@This()) !?Ast.BindingModifier {
    if (!self.is_next_one_of(.{.keyword_alias, .keyword_mut, .keyword_move})) return null;
    const tok = try self.next();
    const binding_kind = switch(tok.tag) {
        .keyword_alias => .alias,
        .keyword_mut => .mut,
        .keyword_move => .move,
    };
    return .{
        .span = tok.span,
        .kind = binding_kind,
    };
}

fn function_declaration(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (!try self.expect(.keyword_fn)) {
        //FATAL: This should never be null
    }

    const fn_ident = try self.expect_ret(.ident);
    if (fn_ident == null) {
        //ERROR: Expected identifier after 'fn'
    }

    if (!try self.expect(.open_paren)) {
        //ERROR: Expected open parenthesis after identifier
    }
    var is_inline: bool = false;
    var param_tys: std.ArrayList(AstNodeId) = .empty;
    var param_ids: std.ArrayList(Ast.BindingId) = .empty;
    var generics: std.ArrayList(Ast.Generic) = .empty;
    var ret_ty: ?AstNodeId = null;
    while (!self.is_next(.close_paren) and !self.is_next_one_of(.{.ident, .keyword_move, .keyword_alias, .keyword_mut})) {
        if (self.is_next(.dollar)) {
            const dollar_tok = self.next() catch unreachable;
            _ = dollar_tok;
            const id = try self.expect_ret(.ident);
            var expr: ?AstNodeId = null;
            if (id == null) {
                //ERROR: Expected identifier after '$'
            }
            if (self.is_next(.colon)) {
                const colon_tok = self.next() catch unreachable;
                _ = colon_tok;
                expr = try self.type_expression();
            }
            const generic = Ast.Generic {
                .ident = .{ .span = id.?.span },
                .expr = expr,
            };

            try generics.append(self.allocator, generic);
        } else {
            //Not really sure if this is even possible
            //probably:
            //ERROR: Unexpected token
        }
        _ = self.next_if(.comma);
        if (!self.is_next(.dollar)) break; //Just in case
    }
    while (!self.is_next(.close_paren)) {
        const modifier = try self.binding_modifier();

        const param_ident = try self.next();

        try param_ids.append(self.allocator, .{ .id = .{ .span = param_ident.span }, .modifier = modifier });

        if (self.next_if(.colon)) |colon_token| {
            span.merge(colon_token.span);
            is_inline = true;
            const ty = try self.type_expression();
            try param_tys.append(self.allocator, ty);
        } else if (is_inline) {
            //ERROR: Cannot mix inline and postifx in function declaration
        }
        const has_comma = self.next_if(.comma);
        if (self.is_next_one_of(.{
            .keyword_alias,
            .keyword_move,
            .keyword_mut,
            .ident,
        })) {
            if (has_comma == null) {
                //ERROR: Expected comma in-between paramaters
            }
        } else if (!self.is_next(.close_paren)) {
            //ERROR: Unexpected token
        }
    }
    if (self.is_next(.colon)) {
        const colon_tok = self.next() catch unreachable;
        _ = colon_tok;
        if (is_inline) {
            //ERROR: Cannot mix inline and postfix in function declaration
        }

        if (self.is_next(.open_paren)) {
            const paren_tok = self.next() catch unreachable;
            _ = paren_tok;
            while (!self.is_next(.close_paren)) {
                const ty = try self.type_expression();
                const comma_tok = self.next_if(.comma);
                _ = comma_tok;
                try param_tys.append(self.allocator, ty);
            }
        } else {
            const ty = try self.type_expression();
            try param_tys.append(self.allocator, ty);
        }

    }
    if (self.is_next(.thin_arrow)) {
        const arrow_tok = self.next() catch unreachable;
        _ = arrow_tok;
        ret_ty = try self.type_expression();
    }

    const body = try self.expression_block();

    span.merge(.{ .start = span.start, .end = self.lexer.index, .fileid = self.file });
    const node = Ast.FnDecl {
        .body = body,
        .ident = fn_ident.?,
        .return_ty = ret_ty,
        .param_types = try param_tys.toOwnedSlice(),
        .params = try param_ids.toOwnedSlice(),
        .generics = try generics.toOwnedSlice(),
    };
    
    const nodeid = try self.builder.add_node(.fn_decl, span, node);
    return nodeid;
}


