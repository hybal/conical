//! This file contains the parser for the conical language
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

fn expect_ret(self: *@This(), tag: lex.Tag) !common.Either(lex.Token, diag.ErrorId) {
    if (!self.lexer.is_next_token(tag)) {
        const expected_error = errors.UnexpectedTokenError {
            .found = self.lexer.next_token(),
        };
        const errorid = try self.context.session.push(expected_error.get_error_type());
        return .make(errorid);
    }
    return .make(self.lexer.next_token());
}

fn expect(self: *@This(), tag: lex.Tag) !?diag.ErrorId {
    const out = try self.expect_ret(tag);
    if (out.is_b()) return out.b;
    return null;
}

fn is_next_one_of(self: *@This(), comptime values: anytype) bool {
    const next = self.lexer.peek_token();
    inline for (values) |val| {
        if (next.tag == val) return true;
    }
    return false;
}

fn is_next(self: *@This(), tag: lex.Tag) bool {
    const next = self.lexer.peek_token();
    return next.tag == tag;
}

fn next_if(self: *@This(), tag: lex.Tag) ?lex.Token {
    if (is_next(tag)) return self.lexer.next_token();
    return null;
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
    _ = try self.expect(.semicolon);
    return .{
        .path = path,
    };

}

fn item(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    const vis = try self.visibility();
    const link = try self.linkage();
    const function_mods = try self.function_modifiers();
    const kind = switch (self.lexer.peek_token().tag) {
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
        const tok = self.lexer.next_token();
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
    if (let_keyword.is_b()) {
        const node = Ast.Poison {
            .error_id = let_keyword.b
        };
        const nodeid = try self.builder.add_node(.poison, span, node);
        //Perform Error Recovery
        return nodeid;
    }
    
    const modifier = try self.binding_modifier();

    const ident = try self.expect_ret(.ident);

    if (ident.is_b()) {
        const node = Ast.Poison {
            .error_id = ident.b,
        };
        
        // Error Recovery

        //NOTE: In the future, the span should really be the actual poisoned node instead of everything up to this point
        const nodeid = try self.builder.add_node(.poison, span, node);
        return nodeid;
    }
    var typeexpr: Ast.TypeExpr = null;
    if (self.next_if(.colon)) |tok| {
        span.merge(tok.span);
        typeexpr = try self.type_expression();
    }
    //TODO: Make these kind of syntax errors more explicit
    const eq_tok = try self.expect(.eq);
    if (eq_tok) |errid| {
        const node = Ast.Poison {
            .error_id = errid,
        };

        const nodeid = try self.builder.add_node(.poison, span, node);
        return nodeid;
    }
    const expr = try self.expression();
    span.merge(self.builder.get_span(expr));
    const node = Ast.VarDecl {
        .ident = .{ .span = ident.a.span, .value = "FIXME", },
        .initialize = expr,
        .ty = typeexpr,
        .modifier = modifier,
    };

    const nodeid = try self.builder.add_node(.var_decl, span, node);
    return nodeid;
}

fn binding_modifier(self: *@This()) !?Ast.BindingModifier {
    if (!self.is_next_one_of(.{.keyword_alias, .keyword_mut, .keyword_move})) return null;
    const tok = self.lexer.next_token();
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


