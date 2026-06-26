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
tmp_allocator: std.mem.Allocator = std.heap.page_allocator, //TODO: use an actual allocator
lexer: lex.Lexer,
context: *common.Context,
has_module: bool = false,
builder: Ast.AstBuilder,
file: common.FileId,
saved_token: ?lex.Token = null,
previous_token: lex.Token = undefined,



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
pub fn init(context: *common.Context, buffer: []const u8, file: common.FileId, gpa: std.mem.Allocator) !@This() {
    return .{
        .lexer = try lex.Lexer.init(buffer, file),
        .allocator = gpa,
        .context = context,
        .builder= .init(gpa),
        .file = file,
    };
}

pub fn deinit(self: *@This()) void {
    self.builder.deinit();
}


/// Checks the next token, if it equals 'tag' then it returns the token, otherwise it returns null.
/// returns error.EOF at end of stream.
/// Does essentially the same thing as `next_if` just with a different intention.
fn expect_ret(self: *@This(), tag: lex.Tag) !?lex.Token {
    if (self.is_next(tag)) {
        return try self.next();
    }
    return null;
}

/// Checks to see if the next token is `tag`, and if it is advances the lexer.
/// Returns true if present false otherwise.
/// Returns error.EOF if at end of stream.
fn expect(self: *@This(), tag: lex.Tag) !bool {
    if (!self.is_next(tag)) {
        return false;
    }
    _ = try self.next();
    return true;
}

/// Checks all tags in `values` to see if any of them match the next token.
/// If it does it just returns whether it found one, it does not advance the lexer.
fn is_next_one_of(self: *@This(), comptime values: anytype) bool {
    const next_tok_option = self.peek();
    if (next_tok_option == null) return false;
    const next_tok = next_tok_option.?;
    inline for (values) |val| {
        if (next_tok.tag == val) return true;
    }
    return false;
}


/// Checks if the next token is `tag`, returns true if so, false otherwise.
/// Does not advance the lexer.
fn is_next(self: *@This(), tag: lex.Tag) bool {
    const next_tok = self.peek();
    if (next_tok == null) return false;
    return next_tok.?.tag == tag;
}

/// Checks if the next token is `tag`, returns it if so, null otherwise.
/// Does advance the lexer if correct token.
fn next_if(self: *@This(), tag: lex.Tag) ?lex.Token {
    if (self.is_next(tag)) return self.next() catch unreachable;
    return null;
}

/// Advance the lexer and return the next token.
/// Returns error.EOF if at end of input.
/// Respects `restore`
fn next(self: *@This()) !lex.Token {
    if (self.saved_token) |prev| {
        self.saved_token = null;
        self.previous_token = prev;
        self.lexer.index = prev.span.end;
        return prev;
    }
    const out = self.lexer.next_token();
    if (out.tag == .eof) {
        return error.EOF;
    }
    self.previous_token = out;
    return out;
}

/// Resets the lexer to right before `tok` and saves `tok`. 
/// FIXME: Since the lexer is being reset, there isn't really a need to save the token in addition.
fn restore(self: *@This(), tok: lex.Token) void {
    self.saved_token = tok;
    self.lexer.index = tok.span.start;
}

/// Gets the next token without advancing the lexer.
/// If the lexer returns .eof, returns null.
/// Respects `restore`
fn peek(self: *@This()) ?lex.Token {
    if (self.saved_token) |prev| {
        return prev;
    }
    const next_tok = self.lexer.peek_token();
    if (next_tok.tag == .eof) return null;
    return next_tok;
}


/// The entrypoint for the parser
/// Returns the fully built Ast.
/// Returns error.EOF if it runs out of input unexpectedly.
/// Returns other allocator errors.
pub fn parse(self: *@This()) !Ast.Ast {
    return try self.program();
}

// ---- START TOP-LEVEL ----

/// Parser module declaration and any number of top-level declarations.
/// Corresponds to grammar rule `PROGRAM`
fn program(self: *@This()) anyerror!Ast.Ast {
    // Every source file requires a module declaration as the first thing in the file.
    // MODULE_DECLARATION
    const mod = try self.module_declaration();
    var decls = std.ArrayList(AstNodeId).empty;
    // ITEM*
    while (self.lexer.has_next()) {
        const decl = try self.item();
        try decls.append(self.allocator, decl);
    }
    const prog = Ast.Program {
        .declarations = try decls.toOwnedSlice(self.allocator),
        .module = mod,
    };

    self.builder.set_program(prog);
    defer self.deinit();
    return self.builder.build();
}

/// Parses a module declaration
/// Corresponds to grammar rule `MODULE_DECLARATION`
fn module_declaration(self: *@This()) !?Ast.ModuleDecl {
    var span: common.Span = .init(self.lexer.index, self.file);
    // KEYWORD_MOD
    if (!(try self.expect(.keyword_mod))) {
        const err = errors.ExpectedDeclarationError {
            .span = span,
            .ty = .expected_module,
        };
        const errid = try self.context.session.push(try err.get_error_type(self.allocator));
        _ = errid;
        while (self.is_next_one_of(.{ .ident, .colon2, .semicolon })) {
            _ = try self.next();
        }
        return null;
    }
    // EXPRESSION_PATH
    const path = try self.expression_path();
    // ';'
    if(!(try self.expect(.semicolon))) {
        const err = errors.ExpectedTokenError {
            .expected = .semicolon,
            .span = .init(self.builder.get_span(path).end-1, self.file),
        };
        const errid = try self.context.session.push(try err.get_error_type(self.allocator));
        _ = errid;
    }
    span.merge(.init(self.lexer.index, self.file));
    return .{
        .path = path,
        .span = span,
    };

}

// Parses a top-level declaration / item
// Corresponds to grammar rule `ITEM`
fn item(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    // currently only `pub`
    const vis = try self.visibility();
    // `extern`/`export`
    const link = try self.linkage();
    // We parse function modifiers here to make things easier.
    // TODO: move this to function parsing
    const function_mods = try self.function_modifiers();
    const peek_tok = self.peek();
    if (peek_tok == null) {
        return error.EOF;
    }
    // Switches over the next token to dispatch to correct parse rule.
    // Also handles multiple module declarations
    const kind: struct {Ast.ItemKind, AstNodeId }= switch (peek_tok.?.tag) {
        .keyword_fn => .{ .function, try self.function_declaration()},
        .keyword_let => .{.binding, try self.let_binding()},
        .keyword_mod => {
            const decl = try self.module_declaration();
            const sp: common.Span = .init(decl.?.span.start+1, self.file);
            const err = errors.UnexpectedDeclarationError {
                .span = sp,
                .ty = .module_decl,
                .notes = &.{ "there can only be one module per file" },
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            const poison_node = Ast.Poison {
                .error_id = errid,
            };
            const poisonid = try self.builder.add_node(.poison, sp, poison_node);
            return poisonid;
        },
        else =>  {
            const err = errors.UnexpectedTokenError {
                .found = peek_tok.?,
                .notes = &.{ "top-level can only contain functions, imports, and variables" },
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            while (!self.is_next_one_of(.{ .keyword_fn, .keyword_let, .keyword_import, .semicolon })) {
                _ = self.next() catch unreachable;
            }
            span.merge(.init(self.lexer.index, self.file));
            const poison_node = Ast.Poison {
                .error_id = errid,
            };
            const poisonid = try self.builder.add_node(.poison, span, poison_node);
            return poisonid;


        },
    };

    // We don't really need to do this, but its here.
    if (link) |l| span.merge(l.span);
    if (vis) |v| span.merge(v.span);
    for (function_mods) |i| {
        span.merge(i.span);
    }

    span.merge(.init(self.lexer.index, self.file));
    const node = Ast.Item {
        .item = kind.@"1",
        .item_kind = kind.@"0",
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

// Parses item visibility.
fn visibility(self: *@This()) !?Ast.Visibility {
    if (self.next_if(.keyword_pub)) |v| {
        return .{
            .kind = .public,
            .span = v.span,
        };
    }
    return null;
}

// Parses item linkage
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

// Parses function modifers.
fn function_modifiers(self: *@This()) ![]Ast.FnMod {
    var mods: std.ArrayList(Ast.FnMod) = .empty;
    while (self.is_next_one_of(.{.keyword_inline, .keyword_pure, .keyword_comptime})) {
        const tok = try self.next();
        const kind: Ast.EvalModifier = switch (tok.tag) {
            .keyword_inline => .@"inline",
            .keyword_pure => .pure,
            .keyword_comptime => .@"comptime",
            else => unreachable,
        };
        try mods.append(self.allocator, .{ .kind = kind, .span = tok.span });
    }
    return try mods.toOwnedSlice(self.allocator);
}

// Parses a let binding
// Corresponds to grammar rule `LET_BINDING`
fn let_binding(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    // KEYWORD_LET
    const let_keyword = try self.expect_ret(.keyword_let);
    if (let_keyword == null) {
        //FATAL: This should never be null, there is a problem in the compiler
        return error.FatalError;
    }
    span.merge(let_keyword.?.span);
    // BINDING_MODIFIER
    const modifier = try self.binding_modifier();

    // IDENT
    const ident_tmp = try self.expect_ret(.ident);
    var ident: common.Either(common.Span, diag.ErrorId) = undefined;
    if (ident_tmp == null) {
        // Expected identifier
        const err = errors.ExpectedTokenError {
            .expected = .ident,
            .span = .init(self.previous_token.span.end, self.file),
        };
        const errid = try self.context.session.push(try err.get_error_type(self.allocator));
        ident = .make(errid);
    } else {
        ident = .make(ident_tmp.?.span);
    }
    // { ':' TYPE_EXPRESSION }
    var typeexpr: ?AstNodeId = null;
    if (self.next_if(.colon)) |tok| {
        span.merge(tok.span);
        typeexpr = try self.type_expression();
        span.merge(self.builder.get_span(typeexpr.?));
    }

    // '='
    if (!try self.expect(.eq)) {
        //ERROR: Expected '='
        const err = errors.ExpectedTokenError {
            .expected = .eq,
            .span = .init(self.previous_token.span.end, self.file),
        };
        const errid = try self.context.session.push(try err.get_error_type(self.allocator));
        _ = errid;
    }
    // EXPRESSION
    const expr = try self.expression();

    // ';'
    const semicolon_tok = try self.expect(.semicolon);
    if (!semicolon_tok) {
        //ERROR: Missing semicolon
        const err = errors.ExpectedTokenError {
            .expected = .semicolon,
            .span = .init(self.previous_token.span.end, self.file)
        };

        const errid = try self.context.session.push(try err.get_error_type(self.allocator));
        _ = errid;
    }
    span.merge(.init(self.lexer.index, self.file));
    const binding_id = Ast.BindingId {
        .id = .{ .span = ident },
        .modifier = modifier,
    };
    const node = Ast.VarDecl {
        .id = binding_id,
        .initialize = expr,
        .ty = typeexpr,
    };

    const nodeid = try self.builder.add_node(.var_decl, span, node);
    return nodeid;
}

/// Parses a binding modifier.
/// Corresponds to grammar rule `BINDING_MODIFIER`
fn binding_modifier(self: *@This()) !?Ast.BindingModifier {
    if (!self.is_next_one_of(.{.keyword_alias, .keyword_mut, .keyword_move})) return null;
    const tok = try self.next();
    const binding_kind: Ast.BindingModifierKind = switch(tok.tag) {
        .keyword_alias => .alias,
        .keyword_mut => .mut,
        .keyword_move => .move,
        else => unreachable,
    };
    return .{
        .span = tok.span,
        .kind = binding_kind,
    };
}

/// Parses a function declaration.
/// Corresponds to grammar rule `FUNCTION_DECLARATION`
fn function_declaration(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    // KEYWORD_FN
    if (!try self.expect(.keyword_fn)) {
        //FATAL: This should never be null
        return error.FatalError;
    }

    // IDENT
    const fn_ident = try self.expect_ret(.ident);
    var fnid: ?common.Either(common.Span, diag.ErrorId) = null;
    if (fn_ident == null) {
        //ERROR: Expected identifier after 'fn'
        const err = errors.ExpectedTokenError {
            .expected = .ident,
            .span = .init(self.previous_token.span.end, self.file),
        };
        const errid = try self.context.session.push(try err.get_error_type(self.allocator));
        fnid = .make(errid);
    } else {
        fnid = .make(fn_ident.?.span);
    }
    // '('
    if (!try self.expect(.open_paren)) {
        //ERROR: Expected open parenthesis after identifier
        const err = errors.ExpectedTokenError {
            .expected = .open_paren,
            .span = span,
        };
        const errid = try self.context.session.push(try err.get_error_type(self.allocator));
        _ = errid;
    }

    // Keep track of if we have come across an inline type, that of `id: type`
    // FIXME: This should only be set on the _first_ parameter, afterwards it should be considered immutable. 
    var is_inline: bool = false;
    var param_tys: std.ArrayList(AstNodeId) = .empty;
    var param_ids: std.ArrayList(Ast.BindingId) = .empty;
    var generics: std.ArrayList(Ast.Generic) = .empty;
    var ret_ty: ?AstNodeId = null;

    // Parse generic list
    // FIXME: This should only be done in the type list, currently it is always in the param list.
    while (self.is_next(.dollar)) {
        // '$'
        const dollar_tok = self.next() catch unreachable;
        _ = dollar_tok;
        // IDENT
        const id = try self.expect_ret(.ident);
        if (id == null) {
            //ERROR: Expected identifier after '$'
            const err = errors.ExpectedTokenError {
                .expected = .ident,
                .span = span,
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            _ = errid;
        }
        // { ':' TYPE_EXPRESSION }
        var expr: ?AstNodeId = null;
        if (self.is_next(.colon)) {
            const colon_tok = self.next() catch unreachable;
            _ = colon_tok;
            expr = try self.type_expression();
        }
        const generic = Ast.Generic {
            .ident = .{ .span = .make(id.?.span) },
            .expr = expr,
        };

        try generics.append(self.allocator, generic);
        // { ',' }
        const comma = self.next_if(.comma);
        if (!self.is_next(.close_paren) and comma == null) {
            //ERROR: Expectected comma
            const err = errors.ExpectedTokenError {
                .expected = .comma,
                .span = span,
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            _ = errid;
        }
    }
    // Parse parameter / type list
    // Determines if the function is using inline or postfix syntax via checking for `id: type`
    while (!self.is_next(.close_paren)) {
        // BINDING_MODIFIER
        const modifier = try self.binding_modifier();
        // IDENT
        const param_ident = try self.expect_ret(.ident);
        var param_ident_errid: ?diag.ErrorId = null;
        if (param_ident == null) {
            //ERROR: Expected identifier
            const err = errors.ExpectedTokenError {
                .expected = .ident,
                .span = span,
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            param_ident_errid = errid;
        }
        const pid: common.Either(common.Span, diag.ErrorId) = if (param_ident_errid) |eid| .make(eid) else .make(param_ident.?.span);
        try param_ids.append(self.allocator, .{ .id = .{ .span = pid }, .modifier = modifier });
        // { ':' TYPE_EXPRESSION }
        // sets the function to inline
        // This could technically make the grammar / parser context-aware,
        //  however since the check is on the first parameter, its more like "dispatch-aware".
        if (self.next_if(.colon)) |colon_token| {
            span.merge(colon_token.span);
            is_inline = true;
            const ty = try self.type_expression();
            try param_tys.append(self.allocator, ty);
        } else if (is_inline) {
            const err = errors.ExpectedTokenError {
                .span = if(param_ident) |p| p.span else .init(self.previous_token.span.end, self.file),
                .expected = .colon,
                .notes = &.{"inline and postfix function declarations cannot be mixed"},
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            _ = errid;
            if (is_inline) {
                const ty = try self.type_expression();
                try param_tys.append(self.allocator, ty);
            }
        }
        const has_comma = self.next_if(.comma);
        if (!self.is_next(.close_paren) and has_comma == null) {
            const err = errors.ExpectedTokenError {
                .span = .init(self.previous_token.span.end, self.file),
                .expected = .comma,
                .help = "add a comma between function parameters",
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            _ = errid;
        }  
    }
    // ')'
    // FIXME: This should have an error case since the loop can stop on EOF.
    _ = self.expect(.close_paren) catch unreachable;
    // postfix list
    if (self.is_next(.colon)) {
        const colon_tok = self.next() catch unreachable;
        if (is_inline) {
            const err = errors.ExpectedTokenError {
                .span = .init(colon_tok.span.end, self.file),
                .expected = .colon,
                .notes = &.{"inline and postfix function declarations cannot be mixed"},
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            _ = errid;
        }
        // '(' ( TYPE_EXPRESSION ( ',' TYPE_EXPRESSION )* ) ')'
        if (self.is_next(.open_paren)) {
            const paren_tok = self.next() catch unreachable;
            _ = paren_tok;
            while (!self.is_next(.close_paren)) {
                const ty = try self.type_expression();
                const comma_tok = self.next_if(.comma);
                _ = comma_tok;
                try param_tys.append(self.allocator, ty);
            }
            _ = self.expect(.close_paren) catch unreachable;
        } else {
            // TYPE_EXPRESSION
            const ty = try self.type_expression();
            try param_tys.append(self.allocator, ty);
        }

    }
    // { '->' TYPE_EXPRESSION }
    if (self.is_next(.thin_arrow)) {
        const arrow_tok = self.next() catch unreachable;
        _ = arrow_tok;
        ret_ty = try self.type_expression();
    }
    // EXPRESSION_BLOCK
    const body = try self.expression_block();

    span.merge(.init(self.lexer.index, self.file));
    const node = Ast.FnDecl {
        .body = body,
        .ident = .{ .span = fnid.? },
        .return_ty = ret_ty,
        .param_types = try param_tys.toOwnedSlice(self.allocator),
        .params = try param_ids.toOwnedSlice(self.allocator),
        .generics = try generics.toOwnedSlice(self.allocator),
    };

    const nodeid = try self.builder.add_node(.fn_decl, span, node);
    return nodeid;
}



// ---- END TOP-LEVEL ----

// ---- START TYPES ----

/// Parses a type expression
/// Corresponds to grammar rule `TYPE_EXPRESSION`
fn type_expression(self: *@This()) !AstNodeId {
    return try self.type_expression_metadata();
}

/// Parses the metadata operator, that is the associated set.
/// Corresponds to grammar rule `TYPE_EXPRESSION_METADATA`
fn type_expression_metadata(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    // TYPE_EXPRESSION_STRICT_INCLUSION
    var left = try self.type_expression_strict_inclusion();
    // ( KEYWORD_WITH TYPE_EXPRESSION_STRICT_INCLUSION )*
    while ( self.is_next(.keyword_with )) {
        const right = try self.type_expression_strict_inclusion();
        const node = Ast.TypeMetadata {
            .left = left,
            .right = right,
            .op = .associative,
        };

        span.merge(.{.start = span.start, .end = self.lexer.index, .fileid = self.file });
        const nodeid = try self.builder.add_node(.type_metadata, span, node);
        left = nodeid;
    }
    return left;
}

/// Parses strict inclusion operators. Also called strict subset / strict superset.
/// Corresponds to grammar rule `TYPE_EXPRESSION_STRICT_INCLUSION`
fn type_expression_strict_inclusion(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    // TYPE_EXPRESSION_MEMBERSHIP
    var left = try self.type_expression_membership();
    // ( ( '<' | '>' ) TYPE_EXPRESSION_MEMBERSHIP )*
    while (self.is_next_one_of(.{ .lt, .gt })) {
        const op = self.next() catch unreachable;
        const right = try self.type_expression_membership();

        const node = Ast.TypeBinaryExpr {
            .left = left,
            .right = right,
            .op = switch (op.tag) {
                .lt => .StrictSubset,
                .gt => .StrictSuperSet,
                else => unreachable,
            },
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.type_binary_expr, span, node);
        left = nodeid;
    }
    return left;
}

/// Parses membership operator also called set inclusion
/// Corresponds to grammar rule `TYPE_EXPRESSION_MEMBERSHIP`
fn type_expression_membership(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    // TYPE_EXPRESSION_DIFFERENCE
    var left = try self.type_expression_difference();
    // ( KEYWORD_IN TYPE_EXPRESSION_DIFFERENCE )*
    while (self.is_next( .keyword_in )) {
        const op = self.next() catch unreachable;
        _ = op;
        const right = try self.type_expression_difference();
        const node = Ast.TypeBinaryExpr {
            .left = left,
            .right = right,
            .op = .Membership,
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.type_binary_expr, span, node);
        left = nodeid;
    }
    return left;
}

/// Parses difference operator, specifically set difference, not symmetric difference.
/// Corresponds to grammar rule `TYPE_EXPRESSION_DIFFERENCE`
fn type_expression_difference(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    // TYPE_EXPRESSION_UNION
    var left = try self.type_expression_union();
    // ( '-' TYPE_EXPRESSION_UNION )*
    while (self.is_next( .minus )) {
        const op = self.next() catch unreachable;
        _ = op;
        const right = try self.type_expression_union();
        const node = Ast.TypeBinaryExpr {
            .left = left,
            .right = right,
            .op = .Difference,
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.type_binary_expr, span, node);
        left = nodeid;
    }
    return left;
}

/// Parses the union operator.
/// Corresponds to grammar rule `TYPE_EXPRESSION_UNION`
fn type_expression_union(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    // TYPE_EXPRESSION_INTERSECTION
    var left = try self.type_expression_intersection();
    // ( '|' TYPE_EXPRESSION_INTERSECTION )*
    while (self.is_next( .pipe )) {
        const op = self.next() catch unreachable;
        _ = op;
        const right = try self.type_expression_intersection();
        const node = Ast.TypeBinaryExpr {
            .left = left,
            .right = right,
            .op = .Union,
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.type_binary_expr, span, node);
        left = nodeid;
    }
    return left;
}

/// Parses intersection operator.
/// Corresponds to grammar rule `TYPE_EXPRESSION_INTERSECTION`
fn type_expression_intersection(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    // TYPE_EXPRESSION_PRODUCT
    var left = try self.type_expression_product();
    // ( '&' TYPE_EXPRESSION_PRODUCT )*
    while (self.is_next( .amp )) {
        const op = self.next() catch unreachable;
        _ = op;
        const right = try self.type_expression_product();
        const node = Ast.TypeBinaryExpr {
            .left = left,
            .right = right,
            .op = .Intersection,
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.type_binary_expr, span, node);
        left = nodeid;
    }
    return left;
}


/// Parses product operator.
/// Corresponds to grammar rule `TYPE_EXPRESSION_PRODUCT`
fn type_expression_product(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    // TYPE_EXPRESSION_MODIFIERS
    var left = try self.type_expression_modifiers();
    // ( '*' TYPE_EXPRESSION_MODIFIERS )*
    while (self.is_next( .star )) {
        const op = self.next() catch unreachable;
        _ = op;
        const right = try self.type_expression_modifiers();
        const node = Ast.TypeBinaryExpr {
            .left = left,
            .right = right,
            .op = .Intersection,
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.type_binary_expr, span, node);
        left = nodeid;
    }
    return left;
}

/// Parses unary modifiers.
/// Corresponds to grammar rule `TYPE_EXPRESSION_MODIFIERS`
/// FIXME: Rename to unary
fn type_expression_modifiers(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    var mods: std.ArrayList(Ast.TypeModifierOp) = .empty;
    // ( '&' | '[]' | '[' EXPRESSION ']' )*
    while (self.is_next_one_of(.{ .amp, .amp2, .open_square })) {
        const tok = self.next() catch unreachable;

        if (tok.tag == .open_square and !self.is_next(.close_square)) {
            const array_expr = try self.expression();
            if (!try self.expect(.close_square)) {
                //ERROR: Expected ]
                const err = errors.ExpectedTokenError {
                    .expected = .close_square,
                    .span = .init(span.end, self.file),
                };
                const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                _ = errid;
            }
            try mods.append(self.allocator, .{ .Array = array_expr });
        } else if (tok.tag == .open_square and self.is_next(.close_square)) {
            try mods.append(self.allocator, .Slice);
        } else if (tok.tag == .amp2) {
            try mods.append(self.allocator, .Reference);
            try mods.append(self.allocator, .Reference);
        } else {
            try mods.append(self.allocator, .Reference);
        }
    }
    // TYPE_EXPRESSION_GROUPING
    const expr = try self.type_expression_grouping();

    const node = Ast.TypeModifier {
        .expr = expr,
        .mods = try mods.toOwnedSlice(self.allocator),
    };
    span.merge(.init(self.lexer.index, self.file));
    const nodeid = try self.builder.add_node(.type_modifier, span, node);
    return nodeid;
}


/// Parses grouping syntax.
/// More accurate to say that it parses things that have the same precedence as grouping.
/// Corresponds to grammar rule `TYPE_EXPRESSION_GROUPING`
fn type_expression_grouping(self: *@This()) anyerror!AstNodeId {
    // '(' TYPE_EXPRESSION ')'
    if (self.is_next(.open_paren)) {
        _ = self.next() catch unreachable;
        const expr = try self.type_expression();
        if (!try self.expect(.close_paren)) {
            const err = errors.ExpectedTokenError {
                .expected = .close_paren,
                .span = .init(self.previous_token.span.end, self.file),
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            _ = errid;
        }
        return expr;
    }
    // '{' EXPRESSION '}'
    // TODO: Perhaps consider making this a normal block?
    if (self.is_next(.open_bracket)) {
        _ = self.next() catch unreachable;
        const expr = try self.expression();
        if (!try self.expect(.close_bracket)) {
            const err = errors.ExpectedTokenError {
                .expected = .close_bracket,
                .span = .init(self.previous_token.span.end, self.file),
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            _ = errid;
        }
        return expr;
    }

    // IDENT | TYPE_EXPRESSION_LABEL
    // This is currently the only place that backtracking is required.
    // TODO: Move this to type_expression_literal
    if (self.is_next(.ident)) {
        const save = self.next() catch unreachable;
        if (self.is_next(.colon)) {
            self.restore(save);
            const out = try self.type_expression_label();
            return out;
        }
        self.restore(save);
        const out = try self.expression_path();
        return out;
    }
    // TYPE_EXPRESSION_SUGAR
    if (self.is_next_one_of(.{ .keyword_struct, .keyword_enum, .keyword_impl })) {
        const expr = try self.type_expression_sugar();
        return expr;
    }

    // TYPE_EXPRESSION_PRIMARY
    return try self.type_expression_primary();
}

/// Parses a label
/// Corresponds to grammar rule `TYPE_EXPRESSION_LABEL`
/// TODO: This can probably be moved inline.
fn type_expression_label(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    const ident = try self.expect_ret(.ident);
    if (ident == null) {
        return error.FatalError;
    }
    if (!try self.expect(.colon)) {
        return error.FatalError;
    }
    const group = try self.type_expression_grouping();
    const node = Ast.TypeLabel {
        .label = .{ .span = .make(ident.?.span) },
        .expr = group,
    };
    span.merge(.init(self.lexer.index, self.file));
    const nodeid = try self.builder.add_node(.type_label, span, node);
    return nodeid;
}


/// Parses all primary expressions
/// Corresponds to grammar rule `TYPE_EXPRESSION_PRIMARY`
/// Note that this and TYPE_EXPRESSION_LITERAL are technically at the same precedence level
///  they are just seperated to make things like ranges more exact.
fn type_expression_primary(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (self.next_if(.dot)) |_| {
        if (self.next_if(.open_bracket)) |_| {
            var elems: std.ArrayList(AstNodeId) = .empty;
            while (!self.is_next(.close_bracket)) {
                const elem = try self.type_expression_literal();
                try elems.append(self.allocator, elem);
            }
            if (!try self.expect(.close_bracket)) {
                //This should never be executed
            }
        }
    }

    if (self.next_if(.keyword_Self)) |_| {
        const node: Ast.TypeLiteral = .self;

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.type_literal, span, node);
        return nodeid;
    }
    return try self.type_expression_literal();

}

/// Parses a literal type expression
/// Corresponds to grammar rule `TYPE_EXPRESSION_LITERAL`
/// TODO: This can probably be merged with EXPRESSION_LITERAL
fn type_expression_literal(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    // '.' IDENT
    // a symbol
    if (self.next_if(.dot)) |_| {
        if (self.is_next(.ident)) {
            const ident = self.next() catch unreachable;
            const node = Ast.TypeLiteral {
                .symbol = .{ .span = .make(ident.span) },
            };
            span.merge(.init(self.lexer.index, self.file));
            const nodeid = try self.builder.add_node(.type_literal, span, node);
            return nodeid;
        }

    }

    // Various literals
    if (self.is_next_one_of(.{
        .int_literal,
        .float_literal,
        .string_literal,
        .raw_string_literal,
        .char_literal,
        .keyword_true,
        .keyword_false,
    })) {
        const tok = self.next() catch unreachable;
        const node = Ast.TypeLiteral {
            .value = tok
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.type_literal, span, node);
        return nodeid;
    }
    //ERROR: Unexpected token
    const err = errors.UnexpectedTokenError {
        .found = self.peek().?,
    };
    const errid = try self.context.session.push(try err.get_error_type(self.allocator));
    const errnode = try self.builder.add_node(.poison, self.peek().?.span, Ast.Poison {
        .error_id = errid
    });
    return errnode;
}

/// Parses various syntax sugar
/// Corresponds to grammar rule `TYPE_EXPRESSION_SUGAR`
fn type_expression_sugar(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    const peek_tok = self.peek();
    if (peek_tok == null) {
        return error.EOF;
    }

    const out = switch (peek_tok.?.tag) {
        .keyword_struct => try self.type_struct_sugar(),
        .keyword_enum => try self.type_enum_sugar(),
        .keyword_impl => try self.type_impl_sugar(),
        else => null,
    };

    if (out == null) {
        const left = try self.type_expression_literal();
        if (self.is_next_one_of(.{ .dot2, .bang })) {
            const tok = self.next() catch unreachable;
            var left_exclude = false;
            if (tok.tag == .bang) {
                left_exclude = true;
                if (!try self.expect(.dot2)) {
                }
            }
            const right = try self.type_expression_literal();
            const right_exclude = self.next_if(.bang) != null;

            const node = Ast.TypeRange {
                .start = left,
                .end = right,
                .start_inclusive = !left_exclude,
                .end_inclusive = !right_exclude,
            };
            span.merge(.init(self.lexer.index, self.file));
            const nodeid = try self.builder.add_node(.type_range, span, node);
            return nodeid;
        }
        return left;
    }
    return out.?;

}

/// Parses the struct syntax sugar
/// Corresponds to the grammar rule `TYPE_STRUCT_SUGAR`
fn type_struct_sugar(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    // KEYWORD_STRUCT
    const keyword = try self.expect(.keyword_struct);
    if (!keyword) {
        // Should never be called without a prefix struct keyword
        return error.FatalError;
    }
    if (!try self.expect(.open_bracket)) {
        const err = errors.ExpectedTokenError {
            .expected = .open_bracket,
            .span = .init(self.previous_token.span.end, self.file),
        };
        const errid = try self.context.session.push(try err.get_error_type(self.allocator));
        _ = errid;
        //TODO: Add context aware error recovery.

    }

    var idents: std.ArrayList(Ast.Ident) = .empty;
    var exprs: std.ArrayList(AstNodeId) = .empty;
    while (!self.is_next(.close_bracket)) {
        // IDENT ':' TYPE_EXPRESSION {','}
        const ident = try self.expect_ret(.ident);
        if (!try self.expect(.colon)) {
            const err = errors.ExpectedTokenError {
                .expected = .colon,
                .span = .init(self.previous_token.span.end, self.file),
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            _ = errid;
        }

        const expr = try self.type_expression();

        _ = self.next_if(.comma);
        //TODO: Add check for missing comma

        try idents.append(self.allocator, .{ .span = .make(ident.?.span) });
        try exprs.append(self.allocator, expr);
    }
    //TODO: Add error handling for this.
    _ = self.expect(.close_bracket) catch unreachable;

    const node = Ast.TypeStruct {
        .field_labels = try idents.toOwnedSlice(self.allocator),
        .field_exprs = try exprs.toOwnedSlice(self.allocator),
    };

    span.merge(.init(self.lexer.index, self.file));

    const nodeid = try self.builder.add_node(.type_struct, span, node);
    return nodeid;
}

/// Parses enum syntax sugar
/// Corresponds to grammar rule `TYPE_ENUM_SUGAR`
fn type_enum_sugar(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (!try self.expect(.keyword_enum)) {
        return error.FatalError;
    }
    if (!try self.expect(.open_bracket)) {
        const err = errors.ExpectedTokenError {
            .expected = .open_bracket,
            .span = .init(self.previous_token.span.end, self.file),
        };
        const errid = try self.context.session.push(try err.get_error_type(self.allocator));
        _ = errid;
    }
    var exprs: std.ArrayList(AstNodeId) = .empty;
    while (!self.is_next(.close_bracket)) {
        const expr = try self.type_expression_literal();
        _ = self.next_if(.comma);
        try exprs.append(self.allocator, expr);
    }
    _ = self.expect(.close_bracket) catch unreachable;

    const node = Ast.TypeEnum {
        .variants = try exprs.toOwnedSlice(self.allocator),
    };

    span.merge(.init(self.lexer.index, self.file));

    const nodeid = try self.builder.add_node(.type_enum, span, node);
    return nodeid;
}

/// Parses impl syntax sugar
/// Corresponds to grammar rule `TYPE_IMPL_SUGAR`
fn type_impl_sugar(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    const keyword = try self.expect(.keyword_impl);
    if (!keyword) return error.FatalError;
    if (!try self.expect(.open_bracket)) {
        const err = errors.ExpectedTokenError {
            .expected = .open_bracket,
            .span = .init(self.previous_token.span.end, self.file),
        };
        const errid = try self.context.session.push(try err.get_error_type(self.allocator));
        _ = errid;

    }
    var decs: std.ArrayList(AstNodeId) = .empty;
    while (!self.is_next(.close_bracket)) {
        const dec = try self.item();
        try decs.append(self.allocator, dec);
    }
    _ = self.expect(.close_bracket) catch unreachable;

    const node = Ast.TypeImpl {
        .declarations = try decs.toOwnedSlice(self.allocator),
    };

    span.merge(.init(self.lexer.index, self.file));
    const nodeid = try self.builder.add_node(.type_impl, span, node);
    return nodeid;
}

// ---- START EXPRESSIONS ----

/// Parses an expression
/// Corresponds to grammar rule `EXPRESSION`
fn expression(self: *@This()) anyerror!AstNodeId {
    // KEYWORD_TYPE TYPE_EXPRESSION
    if (self.next_if(.keyword_type)) |_| {
        return try self.type_expression();
    }
    // | EXPRESSION
    return try self.expression_return();
}

fn expression_return(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (self.next_if(.keyword_return)) |_| {
        const expr = try self.expression();
        const node = Ast.ReturnStmt {
            .expr = expr,
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.return_stmt, span, node);
        return nodeid;
    }
    return try self.expression_if();
}



fn expression_if(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (self.next_if(.keyword_if)) |_| {
        const open_paren = self.next_if(.open_paren);
        const cond = try self.expression();
        if (open_paren != null) {
            if (!try self.expect(.close_paren)) {
                //ERROR: Expected ')'
                span.merge(.init(self.lexer.index, self.file));
                const err = errors.ExpectedTokenError {
                    .expected = .close_paren,
                    .span = .init(span.end, self.file),
                };
                const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                _ = errid;
            }
        }
        var refinements: std.ArrayList(Ast.Refinement) = .empty;
        if (self.next_if(.pipe)) |_| {
            while (!self.is_next(.pipe)) {
                const ident = try self.expect_ret(.ident);
                if (ident == null) {
                    //ERROR: Expected identifier
                    span.merge(.init(self.lexer.index, self.file));
                    const err = errors.ExpectedTokenError {
                        .expected = .ident,
                        .span = .init(span.end, self.file),
                    };
                    const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                    _ = errid;
                }
                var refinement = Ast.Refinement {
                    .a = .{ .span = .make(ident.?.span) },
                    .b = null,
                };
                if (self.next_if(.eq)) |_| {
                    const maps_to = try self.expect_ret(.ident);
                    if (maps_to == null) {
                        //ERROR: expected identifier after '='
                        return error.ParseError;
                    }
                    refinement.b = .{ .span = .make(maps_to.?.span) };
                }
                _ = self.next_if(.comma);
                try refinements.append(self.allocator, refinement);
            }
            _ = self.expect(.pipe) catch unreachable;
        }

        const block = if (open_paren != null) try self.expression_optional_block() else try self.expression_block();

        var else_block: ?AstNodeId = null;

        if (self.next_if(.keyword_else)) |_| {
            else_block = try self.expression_optional_block();
        }
        const node = Ast.IfStmt {
            .block = block,
            .condition = cond,
            .else_block = else_block,
            .refinements = if (refinements.items.len == 0) null else try refinements.toOwnedSlice(self.allocator),
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.if_stmt, span, node);
        return nodeid;

    }
    return try self.expression_match();
}

fn expression_block(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (!try self.expect(.open_bracket)) {
        const err = errors.ExpectedExpressionError {
            .expected = .block,
            .span = .init(self.previous_token.span.end, self.file),
        };
        const errid = try self.context.session.push(try err.get_error_type(self.allocator));
        _ = errid;
    }
    var stmts: std.ArrayList(AstNodeId) = .empty;
    while (!self.is_next(.close_bracket)) {
        const stmt = try self.statement();
        try stmts.append(self.allocator, stmt);
    }
    _ = self.expect(.close_bracket) catch unreachable;

    const node = Ast.Block {
        .exprs = try stmts.toOwnedSlice(self.allocator),
    };

    span.merge(.init(self.lexer.index, self.file));
    const nodeid = try self.builder.add_node(.block, span, node);
    return nodeid;
}

fn expression_optional_block(self: *@This()) !AstNodeId {
    if (self.is_next(.open_bracket)) {
        return try self.expression_block();
    }
    return try self.expression(); //May be changed to statement
}

fn expression_match(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (self.next_if(.keyword_match)) |_| {
        const expr = try self.expression();
        if (!try self.expect(.open_bracket)) {
            const err = errors.ExpectedTokenError {
                .expected = .open_bracket,
                .span = .init(self.previous_token.span.end, self.file),
                .help = "match expressions must always have brackets",
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            _ = errid;
        }
        var match_arms: std.ArrayList(AstNodeId) = .empty;
        while (!self.is_next(.close_bracket)) {
            var patterns: std.ArrayList(AstNodeId) = .empty;
            while (!self.is_next(.fat_arrow)) {
                const pattern = try self.match_pattern();
                _ = self.next_if(.comma);
                try patterns.append(self.allocator, pattern);
            }
            _ = self.expect(.fat_arrow) catch unreachable;
            var captures: std.ArrayList(Ast.Ident) = .empty;
            if (self.next_if(.pipe)) |_| {
                while (!self.is_next(.pipe)) {
                    const ident = try self.expect_ret(.ident);
                    if (ident == null) {
                        //Expected identifier
                    }
                    try captures.append(self.allocator, .{ .span = .make(ident.?.span) });
                }
                _ = self.expect(.pipe) catch unreachable;
            }
            const block = try self.expression_optional_block();

            const match_arm_node = Ast.MatchArm {
                .captures = if (captures.items.len == 0) null else try captures.toOwnedSlice(self.allocator),
                .patterns = if (patterns.items.len == 0) null else try patterns.toOwnedSlice(self.allocator),
                .block = block,
            };

            span.merge(.init(self.lexer.index, self.file));
            const match_arm_nodeid = try self.builder.add_node(.match_arm, span, match_arm_node);

            try match_arms.append(self.allocator, match_arm_nodeid);
        }
        _ = self.expect(.close_paren) catch unreachable;

        const node = Ast.Match {
            .arms = try match_arms.toOwnedSlice(self.allocator),
            .expr = expr,
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.match, span, node);
        return nodeid;
    }
    return try self.expression_logical_or();
}

fn match_pattern(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (self.is_next(.dot)) {
        if (!try self.expect(.open_bracket)) {
            //ERROR: Expected '{'
            span.merge(.init(self.lexer.index, self.file));
            const err = errors.ExpectedTokenError {
                .expected = .open_bracket,
                .span = .init(span.end, self.file),
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            _ = errid;
        }
        var ids: std.ArrayList(Ast.Ident) = .empty;
        var vals: std.ArrayList(AstNodeId) = .empty;
        while (!self.is_next(.close_bracket)) {
            const ident = try self.expect_ret(.ident);
            if (ident == null) {
                //ERROR: Expected identifier
                const err = errors.ExpectedTokenError {
                    .expected = .ident,
                    .span = .init(span.end, self.file),
                };
                const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                _ = errid;
            }

            if (!try self.expect(.colon)) {
                //ERROR: Expected ':'
                return error.ParseError;
            }
            const expr = try self.match_pattern();
            _ = self.next_if(.comma);
            try ids.append(self.allocator, .{ .span = .make(ident.?.span) });
            try vals.append(self.allocator, expr);
        }

        if (!try self.expect(.close_bracket)) return error.ParseError;
        const node = Ast.MatchCompoundLiteral {
            .ids = try ids.toOwnedSlice(self.allocator),
            .values = try vals.toOwnedSlice(self.allocator)
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.match_compound_literal, span, node);
        return nodeid;
    }
    const left = try self.type_expression_literal();
    if (self.is_next_one_of(.{.dot2, .bang})) {
        const left_exclude = self.next() catch unreachable;
        const right_exclude = self.next_if(.bang);
        const right = try self.type_expression_literal();

        const node = Ast.TypeRange {
            .start = left,
            .start_inclusive = left_exclude.tag != .bang,
            .end = right,
            .end_inclusive = right_exclude == null,
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.type_range, span, node);
        return nodeid;
    }
    return left;
}

fn expression_logical_or(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    var left = try self.expression_logical_and();
    while (self.next_if(.pipe2)) |tok| {
        const right = try self.expression_logical_and();
        const node = Ast.BinaryExpr {
            .left = left,
            .right = right,
            .op = tok
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.binary_expr, span, node);
        left = nodeid;
    }
    return left;
}

fn expression_logical_and(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    var left = try self.expression_bitwise_or();
    while (self.next_if(.amp2)) |tok| {
        const right = try self.expression_bitwise_or();
        const node = Ast.BinaryExpr {
            .left = left,
            .right = right,
            .op = tok
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.binary_expr, span, node);
        left = nodeid;
    }
    return left;
}

fn expression_bitwise_or(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    var left = try self.expression_bitwise_xor();
    while (self.next_if(.pipe)) |tok| {
        const right = try self.expression_bitwise_xor();
        const node = Ast.BinaryExpr {
            .left = left,
            .right = right,
            .op = tok
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.binary_expr, span, node);
        left = nodeid;
    }
    return left;
}

fn expression_bitwise_xor(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    var left = try self.expression_bitwise_and();
    while (self.next_if(.caret)) |tok| {
        const right = try self.expression_bitwise_and();
        const node = Ast.BinaryExpr {
            .left = left,
            .right = right,
            .op = tok
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.binary_expr, span, node);
        left = nodeid;
    }
    return left;
}

fn expression_bitwise_and(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    var left = try self.expression_equality();
    while (self.next_if(.amp)) |tok| {
        const right = try self.expression_equality();
        const node = Ast.BinaryExpr {
            .left = left,
            .right = right,
            .op = tok
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.binary_expr, span, node);
        left = nodeid;
    }
    return left;
}

fn expression_equality(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    var left = try self.expression_relational();
    while (self.is_next_one_of(.{.eq2, .bangeq })) {
        const tok = self.next() catch unreachable;
        const right = try self.expression_relational();
        const node = Ast.BinaryExpr {
            .left = left,
            .right = right,
            .op = tok
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.binary_expr, span, node);
        left = nodeid;
    }
    return left;
}

fn expression_relational(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    var left = try self.expression_shift();
    while (self.is_next_one_of(.{.lt, .lteq, .gt, .gteq })) {
        const tok = self.next() catch unreachable;
        const right = try self.expression_shift();
        const node = Ast.BinaryExpr {
            .left = left,
            .right = right,
            .op = tok
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.binary_expr, span, node);
        left = nodeid;
    }
    return left;
}

fn expression_shift(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    var left = try self.expression_additive();
    while (self.is_next_one_of(.{.lt2, .gt2})) {
        const tok = self.next() catch unreachable;
        const right = try self.expression_additive();
        const node = Ast.BinaryExpr {
            .left = left,
            .right = right,
            .op = tok
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.binary_expr, span, node);
        left = nodeid;
    }
    return left;
}

fn expression_additive(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    var left = try self.expression_multiplicative();
    while (self.is_next_one_of(.{.plus, .minus})) {
        const tok = self.next() catch unreachable;
        const right = try self.expression_multiplicative();
        const node = Ast.BinaryExpr {
            .left = left,
            .right = right,
            .op = tok
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.binary_expr, span, node);
        left = nodeid;
    }
    return left;
}

fn expression_multiplicative(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    var left = try self.expression_cast();
    while (self.is_next_one_of(.{.star, .slash, .percent})) {
        const tok = self.next() catch unreachable;
        const right = try self.expression_cast();
        const node = Ast.BinaryExpr {
            .left = left,
            .right = right,
            .op = tok
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.binary_expr, span, node);
        left = nodeid;
    }
    return left;
}

fn expression_cast(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    var left = try self.expression_unary();
    while (self.next_if(.keyword_as)) |_| {
        const right = try self.type_expression();
        const node = Ast.Cast {
            .expr = left,
            .ty = right,
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.cast, span, node);
        left = nodeid;
    }
    return left;
}




fn expression_unary(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (self.is_next_one_of(.{ .minus, .bang, .tilde, .star, .amp, .amp2 })) {
        const tok = self.next() catch unreachable;
        const expr = try self.expression_unary();
        const node = Ast.UnaryExpr {
            .expr = expr,
            .op = tok,
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.unary_expr, span, node);
        return nodeid;
    }
    return self.expression_postfix();
}

fn expression_postfix(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    const expr = try self.expression_sequence();

    if (self.next_if(.open_paren)) |_| {
        _ = self.next() catch unreachable;

        var args: std.ArrayList(Ast.FnArg) = .empty;
        while (!self.is_next(.close_paren)) {
            var is_generic = false;
            var ident: ?lex.Token = null;
            if (self.next_if(.dot)) |_| {
                if (self.next_if(.dollar)) |_| {
                    is_generic = true;
                }
                ident = try self.expect_ret(.ident);
                if (ident == null) {
                    //ERROR: expected identifier
                    return error.ParseError;
                }
            }
            const val = try self.expression();
            _ = self.next_if(.comma);
            try args.append(self.allocator, Ast.FnArg {
                .id = .{ .span = .make(ident.?.span) },
                .is_generic = is_generic,
                .val = val,
            });
        }
        _ = try self.expect(.close_paren);

        const node = Ast.FnCall {
            .left = expr,
            .params = try args.toOwnedSlice(self.allocator),
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.fn_call, span, node);
        return nodeid;
    }

    if (self.next_if(.open_square)) |_| {
        _ = self.next() catch unreachable;

        const left = try self.expression();
        if (self.next_if(.dot2)) |_| {
            var right: ?AstNodeId = null;
            if (!self.is_next(.close_square)) {
                right = try self.expression();
            }
            if (!try self.expect(.close_square)) {
                //ERROR: Expected ']'
                return error.ParseError;
            }
            const node = Ast.SliceOp {
                .expr = expr,
                .left = left,
                .right = right,
            };

            span.merge(.init(self.lexer.index, self.file));
            const nodeid = try self.builder.add_node(.slice, span, node);
            return nodeid;
        }
        if (!try self.expect(.close_square)) {
            //ERROR: Expected ']'
            return error.ParseError;
        }

        const node = Ast.IndexOp {
            .expr = expr,
            .index = left,
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.index, span, node);
        return nodeid;
    }

    if (self.next_if(.dot)) |_| {
        _ = self.next() catch unreachable;
        const ident = if (self.is_next_one_of(.{ .ident, .int_literal })) self.next() catch unreachable else null;
        if (ident == null) {
            //ERROR: Expected identifier
            return error.ParseError;
        }

        const node = Ast.AccessOperator {
            .left = expr,
            .right = ident.?,
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.access_operator, span, node);
        return nodeid;
    }
    return expr;
}

fn expression_sequence(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    const expr = try self.expression_initializer();
    if (self.next_if(.pipearrow)) |tok| {
        const right = try self.expression();

        const node = Ast.BinaryExpr {
            .left = expr,
            .right = right,
            .op = tok,
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.binary_expr, span, node);
        return nodeid;
    }
    return expr;
}

fn expression_initializer(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (self.next_if(.dot)) |_| {
        var ty: ?AstNodeId = null;
        if (self.next_if(.open_paren)) |_| {
            ty = try self.type_expression();
            if (!try self.expect(.close_paren)) {
                //ERROR: Expected ')'
                return error.ParseError;
            }
        }
        if (!try self.expect(.open_bracket)) {
            //ERROR: Expected '{'
            return error.ParseError;
        }
        var fields: std.ArrayList(Ast.InitializerField) = .empty;
        while (!self.is_next(.close_bracket)) {
            var ident: ?Ast.Ident = null;
            if (self.next_if(.dot)) |_| {
                const id = try self.expect_ret(.ident);
                if (id == null) {
                    //ERROR: Expected identifier
                    return error.ParseError;
                }
                ident = .{ .span = .make(id.?.span) };
                if (!try self.expect(.eq)) {
                    //ERROR: Expected =
                    return error.ParseError;
                }
            }

            const comma = self.next_if(.comma);
            if (comma == null and !self.is_next(.close_bracket)) {
                //ERROR: Expected }
                return error.ParseError;
        }
        const expr = try self.expression();
        try fields.append(self.allocator, Ast.InitializerField {
            .id = ident,
            .value = expr,
        });
    }
    _ = try self.expect(.close_bracket);

    const node = Ast.Initializer {
        .ty = ty,
        .fields = try fields.toOwnedSlice(self.allocator),
    };

    span.merge(.init(self.lexer.index, self.file));
    const nodeid = try self.builder.add_node(.initializer, span, node);
    return nodeid;
}
return try self.expression_primary();
}

fn expression_primary(self: *@This()) !AstNodeId {
    if (self.next_if(.open_paren)) |_| {
        const expr = try self.expression();
        if (!try self.expect(.close_paren)) {
            //ERROR: Expected ')'
            return error.ParseError;
        }
        return expr;
    }

    if (self.is_next(.back_slash)) {
        return try self.expression_lambda();
    }
    
    if (self.is_next_one_of(.{ .keyword_comptime, .keyword_inline, .keyword_pure })) {
        return try self.expression_mod_block();
    }

    if (self.is_next(.open_bracket)) {
        return try self.expression_block();
    }
    return try self.expression_terminal();
}

fn expression_mod_block(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    const mod: Ast.EvalModifier = switch ((self.next() catch unreachable).tag) {
        .keyword_comptime => .@"comptime",
        .keyword_inline => .@"inline",
        .keyword_pure => .pure,
        else => unreachable,
    };
    span.merge(.init(self.lexer.index, self.file));

    const block = try self.expression_optional_block();

    span.merge(.init(self.lexer.index, self.file));
    const mod_block = Ast.ModBlock {
        .block = block,
        .mod = mod,
    };

    span.merge(.init(self.lexer.index, self.file));
    const out = try self.builder.add_node(.mod_block, span, mod_block);
    return out;
}

fn expression_lambda(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);

    if (!try self.expect(.back_slash)) {
        //FATAL: Should never be called without a leading backslash
        return error.FatalError;
    }
    var is_multi = false;
    var is_inline = false;
    var ret_ty: ?AstNodeId = null;
    var param_tys: std.ArrayList(?AstNodeId) = .empty;
    defer param_tys.deinit(self.tmp_allocator);
    var params: std.ArrayList(Ast.LambdaParam) = .empty;
    var generics: std.ArrayList(Ast.Generic) = .empty;

    if (self.next_if(.open_paren)) |_| {
        is_multi = true;
    }
    while (true) {
        if (self.next_if(.dollar)) |_| {
            const id = try self.expect_ret(.ident);
            if (id == null) {
                //ERROR: Expected identifier
                return error.ParseError;
            }
            var gen_expr: ?AstNodeId = null;
            if (self.next_if(.colon)) |_| {
                gen_expr = try self.type_expression();
            }
            try generics.append(self.allocator, .{ 
                .ident = .{.span = .make(id.?.span) },
                .expr = gen_expr,
            });
        } else {
            const bind_mod = try self.binding_modifier();
            const id = try self.expect_ret(.ident);
            if (id == null) {
                //ERROR: Expected identifier
                return error.ParseError;
            }

            var ty: ?AstNodeId = null;
            if (self.next_if(.colon)) |_| {
                if (is_inline) {
                    //ERROR: cannot mix inline and postfix
                    return error.ParseError;
                }
                is_inline = is_multi;
                ty = try self.type_expression();
            }

            const param = Ast.LambdaParam {
                .ident = .{ .span = .make(id.?.span) },
                .ty = ty,
                .mod = bind_mod,
            };
            try params.append(self.allocator, param);
            if (is_inline or !is_multi) {
                try param_tys.append(self.tmp_allocator, ty);
            }
        }
        if (!is_multi) break;
        const comma = self.next_if(.comma);
        if (comma == null and !self.is_next(.close_paren)) {
            //ERROR: Expected ')'
            return error.ParseError;
        }
        if (self.next_if(.close_paren)) |_| break;
    }

    if (!is_inline and self.is_next(.colon)) {
        _ = self.next() catch unreachable;
        if (self.next_if(.open_paren)) |_| {
            while (!self.is_next(.close_paren)) {
                if (self.next_if(.dollar)) |_| {
                    const id = try self.expect_ret(.ident);
                    if (id == null) {
                        //ERROR: Expected identifier
                        return error.ParseError;
                    }
                    var expr: ?AstNodeId = null;
                    if (self.next_if(.colon)) |_| {
                        expr = try self.type_expression();
                    }

                    try generics.append(self.allocator, .{
                        .ident = .{ .span = .make(id.?.span) },
                        .expr = expr,
                    });
                } else {
                    if (self.next_if(.underscore)) |_| {
                        try param_tys.append(self.tmp_allocator, null);
                    } else {
                        try param_tys.append(self.tmp_allocator, try self.type_expression());
                    }
                }
                const comma = self.next_if(.comma);
                if (comma == null and !self.is_next(.close_paren)) {
                    //ERROR: Expected ')'
                    return error.ParseError;
                }
            }
            _ = try self.expect(.close_bracket);

        } else {
            if (self.next_if(.underscore)) |_| {
                try param_tys.append(self.tmp_allocator, null);
            } else {
                try param_tys.append(self.tmp_allocator, try self.type_expression());
            }
        }
    }

    if (self.next_if(.thin_arrow)) |_| {
        ret_ty = try self.type_expression();
    }

    for (0..param_tys.items.len) |i| {
        params.items[i].ty = param_tys.items[i];
    }

    if (!try self.expect(.fat_arrow)) {
        //ERROR: Expected => after lambda declaration
        return error.ParseError;
    }

    const expr = try self.expression_optional_block();

    const node = Ast.Lambda {
        .expr = expr,
        .generics = try generics.toOwnedSlice(self.allocator),
        .params = try params.toOwnedSlice(self.allocator),
        .ret_ty = ret_ty,
    };

    span.merge(.init(self.lexer.index, self.file));
    const nodeid = try self.builder.add_node(.lambda, span, node);
    return nodeid;

}

fn expression_terminal(self: *@This()) !AstNodeId {
    if (self.is_next(.ident)) {
        return try self.expression_path();
    }
    return try self.expression_literal();
}

fn expression_path(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    var idents: std.ArrayList(Ast.Ident) = .empty;
    const first_id = try self.expect_ret(.ident);
    if (first_id == null) {
        std.debug.print("ERROR: Expected identifier\n", .{});
        return error.ParseError;
    }
    try idents.append(self.allocator, .{ .span = .make(first_id.?.span) });
    while (self.next_if(.colon2)) |_| {
        const id = try self.expect_ret(.ident);
        if (id == null) {
            std.debug.print("ERROR: Expected identifier after ::\n", .{});
            return error.ParseError;
        }
        try idents.append(self.allocator, .{ .span = .make(id.?.span) });

    }
    if (idents.items.len == 0) {
        std.debug.print("FATAL: Should never be 0\n", .{});
        return error.FatalError;
    }
    const node = Ast.Path {
        .parts = try idents.toOwnedSlice(self.allocator),
    };

    span.merge(.init(self.lexer.index, self.file));
    const nodeid = try self.builder.add_node(.path, span, node);
    return nodeid;
}

fn expression_literal(self: *@This()) anyerror!AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (self.is_next_one_of(.{
        .int_literal,
        .string_literal,
        .raw_string_literal, //NOTE: Should be removed from the lexer
        .float_literal,
        .char_literal,
        .keyword_true,
        .keyword_false,
    })) {
        const tok = self.next() catch unreachable;
        const node = Ast.Terminal {
            .termtype = .{
                .value = tok,
            },
            .span = tok.span,
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.terminal, span, node);
        return nodeid;
    }

    if (self.next_if(.dot)) |_| {
        const ident = try self.expect_ret(.ident);
        if (ident == null) {
            //ERROR: Expected identifier
            return error.ParseError;
        }

        const node = Ast.Terminal {
            .span = ident.?.span,
            .termtype = .{
                .symbol = .{ .span = .make(ident.?.span) },
            }
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.terminal, span, node);
        return nodeid;
    }

    //ERROR: Unexpected token
    return error.ParseError;
}

// ---- END EXPRESSIONS ---- //

// ---- START STATEMENTS ---- //


//TODO: This should be changed to a precedence based approach instead of dispatch
fn statement(self: *@This()) anyerror!AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (self.is_next_one_of(.{ 
        .keyword_loop,
        .keyword_while,
        .keyword_for,
    })) {
        return try self.loop();
    }

    if (self.is_next_one_of(.{
        .keyword_let,
        .keyword_fn,
        .keyword_type,
        .keyword_export,
        .keyword_pub,
        .keyword_extern,
    })) {
        return try self.item();
    }

    const left = try self.expression();

    if (self.is_next(.semicolon)) {
        const node = Ast.Terminated {
            .expr = left,
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.terminated, span, node);
        return nodeid;
    }

    if (self.is_next_one_of(.{
        .eq,
        .pluseq,
        .minuseq,
        .stareq,
        .slasheq,
        .percenteq,
        .lt2eq,
        .gt2eq,
        .ampeq,
        .pipeeq,
        .careteq,
        .amp2eq,
        .pipe2eq,
    })) {
        const tok = self.next() catch unreachable;
        const expr = try self.expression();
        const node = Ast.Assignment {
            .expr = expr,
            .lvalue = left,
            .op = tok,
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.assignment, span, node);
        return nodeid;
    }
    return left;
}


fn loop(self: *@This()) !AstNodeId {
    if (self.is_next(.keyword_while)) {
        return try self.while_loop();
    }

    if (self.is_next(.keyword_for)) {
        return try self.for_loop();
    }
    var span: common.Span = .init(self.lexer.index, self.file);
    if (self.next_if(.keyword_loop)) |_| {
        const block = try self.loop_block();
        const node = Ast.Loop {
            .block = block,
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.loop, span, node);
        return nodeid;
    }
    //FATAL: Should not be called without a known loop type
    return error.FatalError;
}

fn loop_block(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (!try self.expect(.open_bracket)) {
        //ERROR: Expected {
        return error.ParseError;
    }
    var stmts: std.ArrayList(AstNodeId) = .empty;
    while (!self.is_next(.close_bracket)) {
        if (self.is_next_one_of(.{ .keyword_break, .keyword_continue })) {
            const tok = self.next() catch unreachable;
            const cont: Ast.LoopControlType = if (tok.tag == .keyword_break) .@"break" else .@"continue";
            const node = Ast.LoopControl {
                .control = cont,
            };

            const nodeid = try self.builder.add_node(.loop_control, span, node);
            try stmts.append(self.allocator, nodeid);
        } else {
            try stmts.append(self.allocator, try self.statement());
        }
    }

    if (self.next() catch null == null) {
        //ERROR: EOF
        return error.ParseError;
    }

    const node = Ast.Block {
        .exprs = try stmts.toOwnedSlice(self.allocator),
    };
    span.merge(.init(self.lexer.index, self.file));
    const nodeid = try self.builder.add_node(.block, span, node);
    return nodeid;

}

fn while_loop(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (!try self.expect(.keyword_while)) {
        //FATAL: Called without a known while loop
        return error.FatalError;
    }

    const cond = try self.expression();
    const block = try self.loop_block();

    const node = Ast.WhileLoop {
        .block = block,
        .condition = cond,
    };

    span.merge(.init(self.lexer.index, self.file));
    const nodeid = try self.builder.add_node(.while_loop, span, node);
    return nodeid;
}

fn for_loop(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (!try self.expect(.keyword_for)) {
        //FATAL: Called without a known for loop
        return error.FatalError;
    }

    const ident = try self.expect_ret(.ident);
    if (ident == null) {
        //ERROR: Expected identifier
        return error.ParseError;
    }

    if (!try self.expect(.keyword_in)) {
        //ERROR: Expected 'in'
        return error.ParseError;
    }

    const expr = try self.expression();

    const block = try self.loop_block();

    const node = Ast.ForLoop {
        .expr = expr,
        .ident = .{ .span = .make(ident.?.span) },
        .block = block
    };

    span.merge(.init(self.lexer.index, self.file));
    const nodeid = try self.builder.add_node(.for_loop, span, node);
    return nodeid;
}
