//! The parser for Conical


//NOTES:
//In the self-hosted rewrite the parser will actually produce a syntax tree for better span tracking instead of an Ast
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
    self.previous_token = tok;
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
    std.debug.assert(next_tok.tag == self.lexer.peek_token().tag);
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

    var items = std.ArrayList(AstNodeId).empty;
    // ITEM*
    while (self.peek() != null and self.lexer.has_next()) {
        const decl = try self.item();
        try items.append(self.allocator, decl);
    }
    const prog = Ast.Program {
        .items = try items.toOwnedSlice(self.allocator),
    };

    self.builder.set_program(prog);
    defer self.deinit();
    return self.builder.build();
}

// Parses a top-level declaration / item
// Corresponds to grammar rule `ITEM`
fn item(self: *@This()) !AstNodeId {

    var span: common.Span = .init(self.lexer.index, self.file);
    const attributes = try self.try_parse_attributes();
    if (self.next_if(.keyword_mod)) |key_tok| {
        const expr = try self.expression();
        if (!try self.expect(.semicolon)) {
            const err = errors.ExpectedTokenError {
                .expected = .semicolon,
                .span = .init(key_tok.span.end, self.file),
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            _ = errid;
        }
        const mod_node = Ast.ModStmt {
            .expression = expr,
        };
        span.merge(.init(self.lexer.index, self.file));
        const mod_nodeid = try self.builder.add_node(.mod_stmt, span, mod_node);
        const node = Ast.Item {
            .item = mod_nodeid,
            .item_kind = .mod_expr,
            .linkage = null,
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.item, span, node);
        return nodeid;
    }
    // `extern`/`export`
    const link = try self.linkage();
    // We parse function modifiers here to make things easier.
    // TODO: move this to function parsing
    const peek_tok = self.peek();
    if (peek_tok == null) {
        return error.EOF;
    }
    // Switches over the next token to dispatch to correct parse rule.
    // Also handles multiple module declarations
    const kind: struct {Ast.ItemKind, AstNodeId }= switch (peek_tok.?.tag) {
        .keyword_fn => .{ .function, try self.function_declaration()},
        .keyword_let => .{ .binding, try self.let_binding()},
        else => |tok| {
            const err = errors.UnexpectedTokenError {
                .found = peek_tok.?,
                .notes = &.{ "top-level can only contain functions, imports, and variables" },
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            _ = self.next() catch {};
            while (!self.is_next_one_of(.{ .keyword_fn, .keyword_let, .keyword_import, .keyword_mod, .eq, .keyword_while, .keyword_for, .keyword_match})) {
                _ = self.next() catch break;
            }
            span.merge(.init(self.lexer.index, self.file));
            const poison_node = Ast.Poison {
                .error_id = errid,
            };
            const poisonid = try self.builder.add_node(.poison, span, poison_node);
            std.debug.print("ERROR: Unexpected token: {any}\n", .{tok});
            return poisonid;


        },
    };

    // We don't really need to do this, but its here.
    if (link) |l| span.merge(l.span);

    span.merge(.init(self.lexer.index, self.file));
    const node = Ast.Item {
        .item = kind.@"1",
        .item_kind = kind.@"0",
        .linkage = link,
    };

    const nodeid = try self.builder.add_node(
        .item,
        span,
        node,
    );

    const out = if (attributes) |attribs| blk: {
        const ats = Ast.AttributeList {
            .attributes = attribs,
            .value = nodeid,
        };

        const atsid = try self.builder.add_node(.attribute_list, span, ats);
        break :blk atsid;
    } else nodeid;

    return out;

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
    if (!try self.expect(.semicolon)) {
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

    if (!try self.expect(.keyword_fn)) {
        // FATAL: this function should never be called without a 'fn' token
        return error.FATAL;
    }

    const id = try self.expect_ret(.ident);
    if (id == null) {
        const err = errors.ExpectedTokenError {
            .expected = .ident,
            .span = .init(self.previous_token.span.end, self.file),
            .help = "Functions need a name"
        };
        const errid = try self.context.session.push(try err.get_error_type(self.allocator));
        const errnode = try self.builder.add_node(.poison, span, Ast.Poison { .error_id = errid });
        return errnode;
    }
    span.merge(.init(self.lexer.index, self.file));

    if (!try self.expect(.open_paren)) {
        const err = errors.ExpectedTokenError {
            .expected = .open_paren,
            .span = .init(self.previous_token.span.end, self.file),
            .help = "Missing parameter list or parenthesis",
        };
        const errid = try self.context.session.push(try err.get_error_type(self.allocator));
        _ = errid;
    }

    var params: std.ArrayList(Ast.FnParam) = .empty;
    while (!self.is_next(.close_paren)) {
        const modifier = try self.binding_modifier();
        const is_generic = self.next_if(.dollar) != null;
        if (self.next_if(.ident)) |param_id| {
            var type_expr: ?AstNodeId = null;
            if (!is_generic) {
                if (!try self.expect(.colon)) {
                    const err = errors.ExpectedTokenError {
                        .expected = .colon,
                        .span = .init(param_id.span.end, self.file),
                    };

                    const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                    _ = errid;
                }

                type_expr = try self.type_expression();
            } else {
                if (self.next_if(.colon)) |_| {
                    type_expr = try self.type_expression();
                }
            }
            const bind_id = Ast.BindingId {
                .id = .{ .span = .make_a(param_id.span) },
                .modifier = modifier,
            };


            if (!self.is_next(.comma) and !self.is_next(.close_paren)) {
                const err = errors.ExpectedTokenError {
                    .expected = .comma,
                    .span = .init(self.previous_token.span.end, self.file),
                    .help = "Expected comma between function parameters",
                };
                const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                _ = errid;
            } else {
                _ = self.next_if(.comma);
            }
            const param = Ast.FnParam {
                .id = bind_id,
                .is_generic = is_generic,
                .mod = modifier,
                .ty = type_expr,
            };

            try params.append(self.allocator, param);
        } 
    }
    _ = try self.expect(.close_paren);

    const ret_ty = if (!self.is_next_one_of(.{.fat_arrow, .semicolon})) try self.type_expression() else null;

    var expr: ?AstNodeId = null;

    if (self.next_if(.fat_arrow)) |_| {
        // This is slightly fragile, if any other form of syntax is added to blocks this will break
        if (!self.is_next_one_of(.{ .open_bracket, .at })) {
            expr = try self.expression();
            if (!try self.expect(.semicolon)) {
                const err = errors.ExpectedTokenError {
                    .expected = .semicolon,
                    .span = .init(self.previous_token.span.end, self.file),
                    .help = "Inline expressions must end with a semicolon",
                };

                const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                _ = errid;
            }
        } else {
            expr = try self.expression_block();

            if (self.next_if(.semicolon)) |tok| {
                const err = errors.UnexpectedTokenError {
                    .found = tok,
                    .notes = &[1][]const u8{ "you shouldn't end a block in a semicolon" },
                };
                const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                _ = errid;
            }
        }
    } else {
        if (!try self.expect(.semicolon)) {
            const err = errors.ExpectedTokenError {
                .expected = .fat_arrow,
                .span = .init(self.previous_token.span.end, self.file),
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            _ = errid;

            if (self.is_next_one_of(.{ .open_bracket, .at })) {
                _ = try self.expression_block();
            }
        }
    }

    const node = Ast.FnDecl {
        .ident = .{ .span = .make_a(id.?.span) },
        .body = expr,
        .params = try params.toOwnedSlice(self.allocator),
        .return_ty = ret_ty,
    };

    span.merge(.init(self.lexer.index, self.file));
    const nodeid = try self.builder.add_node(.fn_decl, span, node);

    return nodeid;


}



// ---- END TOP-LEVEL ----

// ---- START TYPES ----

/// Parses a type expression
/// Corresponds to grammar rule `TYPE_EXPRESSION`
fn type_expression(self: *@This()) anyerror!AstNodeId {

    return try self.type_expression_metadata();
}

/// Parses the metadata operator, that is the associated set.
/// Corresponds to grammar rule `TYPE_EXPRESSION_METADATA`
fn type_expression_metadata(self: *@This()) !AstNodeId {

    var span: common.Span = .init(self.lexer.index, self.file);
    // TYPE_EXPRESSION_STRICT_INCLUSION
    var left = try self.type_expression_prefix();
    // ( KEYWORD_WITH TYPE_EXPRESSION_STRICT_INCLUSION )*
    while ( self.next_if(.keyword_with )) |_| {
        const right = try self.type_expression_prefix();
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

fn type_expression_prefix(self: *@This()) !AstNodeId {

    var span: common.Span = .init(self.lexer.index, self.file);
    if (self.next_if(.keyword_rel)) |_| {
        span.merge(.init(self.lexer.index, self.file));
        const expr = try self.type_expression();
        const node: Ast.TypePrefix = .{
            .kind = .relevant,
            .expr = expr,
        };
        const nodeid = try self.builder.add_node(.type_prefix, span, node);
        return nodeid;
    }
    return try self.type_expression_default();
}

fn type_expression_default(self: *@This()) !AstNodeId {

    var span: common.Span = .init(self.lexer.index, self.file);
    const left = try self.type_expression_strict_inclusion();
    if ( self.is_next(.coloneq) ) {
        
        _ = self.expect(.coloneq) catch unreachable;
        const right = try self.expression();
        const node = Ast.TypeDefault {
            .left = left,
            .right = right,
        };
        span.merge(.init(self.lexer.index, self.file));
        const out = try self.builder.add_node(.type_default, span, node);
        return out;
    }
    return left;
}

/// Parses strict inclusion operators. Also called strict subset / strict superset.
/// Corresponds to grammar rule `TYPE_EXPRESSION_STRICT_INCLUSION`
/// FIXME: Somehow non-strict inclusion just disappeared?
/// Not sure if it just wasnt added or not
fn type_expression_strict_inclusion(self: *@This()) !AstNodeId {

    var span: common.Span = .init(self.lexer.index, self.file);
    // TYPE_EXPRESSION_MEMBERSHIP
    const left = try self.type_expression_inclusion();
    // ( ( '<' | '>' ) TYPE_EXPRESSION_MEMBERSHIP )*
    while (self.is_next_one_of(.{ .lt, .gt })) {
        const op = self.next() catch unreachable;
        const right = try self.type_expression_inclusion();

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
        return nodeid;
    }
    return left;
}

fn type_expression_inclusion(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);

    const left = try self.type_expression_membership();
    while (self.is_next_one_of(.{ .lteq, .gteq })) {
        const op = self.next() catch unreachable;
        const right = try self.type_expression_membership();

        const node = Ast.TypeBinaryExpr {
            .left = left,
            .right = right,
            .op = switch (op.tag) {
                .lteq => .Subset,
                .gteq => .SuperSet,
                else => unreachable,
            },
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.type_binary_expr, span, node);
        return nodeid;
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
    var left = try self.type_expression_range();
    // ( '*' TYPE_EXPRESSION_MODIFIERS )*
    while (self.is_next( .star )) {
        const op = self.next() catch unreachable;
        _ = op;
        const right = try self.type_expression_range();
        const node = Ast.TypeBinaryExpr {
            .left = left,
            .right = right,
            .op = .Product,
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.type_binary_expr, span, node);
        left = nodeid;
    }
    return left;
}

fn type_expression_range(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    const left = try self.type_expression_unary();
    if (self.is_next_one_of(.{ .dot2, .bang })) {
        const tok = self.next() catch unreachable;
        var left_exclude = false;
        if (tok.tag == .bang) {
            left_exclude = true;
            if (!try self.expect(.dot2)) {
                return error.ParserError;
            }
        }
        const right = try self.type_expression_unary();
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
/// Parses unary modifiers.
/// Corresponds to grammar rule `TYPE_EXPRESSION_MODIFIERS`
fn type_expression_unary(self: *@This()) !AstNodeId {

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
    const expr = try self.type_expression_postfix();
    if (mods.items.len > 0) {
        const node = Ast.TypeModifier {
            .expr = expr,
            .mods = try mods.toOwnedSlice(self.allocator),
        };
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.type_modifier, span, node);
        return nodeid;
    }
    return expr;
}

fn type_expression_postfix(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);

    const expr = try self.type_expression_grouping();

    if (self.next_if(.open_paren)) |_| {
        var args: std.ArrayList(Ast.FnArg) = .empty;
        while (!self.is_next(.close_paren)) {
            var name: ?Ast.Ident = null;
            var is_generic: bool = false;

            if (self.next_if(.dot)) |_| {
                if (self.next_if(.dollar)) |_| {
                    is_generic = true;
                }
                const id = try self.expect_ret(.ident);
                if (id == null) {
                    const err = errors.ExpectedTokenError {
                        .expected = .ident,
                        .span = .init(self.previous_token.span.end, self.file)
                    };
                    const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                    return errid;
                }
                name = .{ .span = .make_a(id.?.span) };
            }
            const arg_expr = try self.expression();
            if (!self.is_next(.comma) and !self.is_next(.close_paren)) {
                const err = errors.ExpectedTokenError {
                    .expected = .comma,
                    .span = .init(self.previous_token.span.end, self.file),
                };
                span.merge(.init(self.lexer.index, self.file));
                const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                _ = errid;
            }
            const arg = Ast.FnArg {
                .id = name,
                .is_generic = is_generic,
                .val = arg_expr,
            };
            try args.append(self.allocator, arg);
        }
        _ = try self.expect(.close_paren);
        const node = Ast.FnCall {
            .left = expr,
            .params = try args.toOwnedSlice(self.allocator),
        };

        const nodeid = try self.builder.add_node(.fn_call, span, node);
        return nodeid;
    }
    return expr;
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
    if (self.is_next(.open_bracket)) {
        return try self.expression_block();
    }

    // IDENT | TYPE_EXPRESSION_LABEL
    // This is currently the only place that backtracking is required.
    if (self.is_next(.ident)) {
        const save = self.next() catch unreachable;
        if (self.is_next(.colon)) {
            self.restore(save);
            const out = try self.type_expression_label();
            return out;
        }
        const id_node = Ast.Terminal {
            .termtype = .{ .value = save },
        };
        const out = try self.builder.add_node(.terminal, save.span, id_node);
        return out;
    }
    // TYPE_EXPRESSION_SUGAR
    if (self.is_next_one_of(.{ .keyword_struct, .keyword_enum, .keyword_impl, .keyword_interface })) {
        const expr = try self.type_expression_sugar();
        return expr;
    }

    // TYPE_EXPRESSION_PRIMARY
    return try self.type_expression_primary();
}


/// Parses a label
/// Corresponds to grammar rule `TYPE_EXPRESSION_LABEL`
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
        if (self.next_if(.ident)) |ident| {
            const node = Ast.TypeLiteral {
                .symbol = .{ .span = .make(ident.span) },
            };
            span.merge(.init(self.lexer.index, self.file));
            const nodeid = try self.builder.add_node(.type_literal, span, node);
            return nodeid;
        } else {
            const err = errors.ExpectedTokensError {
                .expected = &[_]lex.Tag{.open_bracket, .ident},
                .span = .init(self.previous_token.span.end, self.file)
            };
            _ = self.next() catch {};
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            return errid;
        }
    }

    if (self.next_if(.keyword_Self)) |_| {
        const node: Ast.TypeLiteral = .self;

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.type_literal, span, node);
        return nodeid;
    }

    if (self.next_if(.keyword_type)) |_| {
        const node: Ast.TypeLiteral = .@"type";
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.type_literal, span, node);
        return nodeid;
    }

    if (self.next_if(.underscore)) |_| {
        const node: Ast.TypeLiteral = .discard;
        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.type_literal, span, node);
        return nodeid;
    }
    return try self.expression_literal();

}

/// Parses various syntax sugar
/// Corresponds to grammar rule `TYPE_EXPRESSION_SUGAR`
fn type_expression_sugar(self: *@This()) !AstNodeId {

    const peek_tok = self.peek();
    if (peek_tok == null) {
        return error.EOF;
    }

    const out = switch (peek_tok.?.tag) {
        .keyword_struct => try self.type_struct_sugar(),
        .keyword_enum => try self.type_enum_sugar(),
        .keyword_impl => try self.type_impl_sugar(),
        .keyword_interface => try self.type_interface_sugar(),
        else => unreachable,
    };
    return out;

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
        var expr: AstNodeId = undefined;
        if (self.is_next(.ident)) {
            const id = try self.next();
            if (self.is_next(.colon)) {
                self.restore(id);
                expr = try self.type_expression_label();
            } else {
                const node = Ast.Terminal {
                    .termtype = .{ .value = id },
                };
                expr = try self.builder.add_node(.terminal, id.span, node);
            }
        } else {
            expr = try self.expression_literal();
        }
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

fn type_interface_sugar(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (!try self.expect(.keyword_interface)) {
        return error.Fatal;
    }
    if (!try self.expect(.open_bracket)) {
        const err = errors.ExpectedTokenError {
            .expected = .open_bracket,
            .span = .init(self.previous_token.span.end, self.file),
        };
        const errid = try self.context.session.push(try err.get_error_type(self.allocator));
        _ = errid;
    }
    var decls: std.ArrayList(AstNodeId) = .empty;
    while (!self.is_next(.close_bracket)) {
        const decl = try self.function_declaration();
        const node = self.builder.get_or_null(Ast.FnDecl, .fn_decl, decl).?;
        if (node.body != null) {
            span.merge(.init(self.lexer.index, self.file));
            // Functions cannot have bodies in interfaces
            return error.ParseError;
        }
        try decls.append(self.allocator, decl);
    }
    _ = try self.expect(.close_bracket);

    const node = Ast.TypeInterface {
        .decls = try decls.toOwnedSlice(self.allocator),
    };

    span.merge(.init(self.lexer.index, self.file));
    const nodeid = try self.builder.add_node(.type_interface, span, node);
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
    return try self.expression_match();
}



fn expression_block(self: *@This()) !AstNodeId {

    var span: common.Span = .init(self.lexer.index, self.file);

    const attribs = try self.try_parse_attributes();
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

    const block = Ast.Block {
        .exprs = try stmts.toOwnedSlice(self.allocator),
    };

    span.merge(.init(self.lexer.index, self.file));
    const nodeid = try self.builder.add_node(.block, span, block);

    const out = if (attribs) |ats| blk: {
        const attrib_node = Ast.AttributeList {
            .attributes = ats,
            .value = nodeid,
        };

        const id = try self.builder.add_node(.attribute_list, span, attrib_node);
        break :blk id;
    } else nodeid;
    return out;
}

fn expression_optional_block(self: *@This()) !AstNodeId {
    if (self.is_next(.open_bracket)) {
        return try self.expression_block();
    }
    return try self.expression();
}

fn expression_match(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);
    if (self.next_if(.keyword_match)) |_| {
        const expr = try self.expression();
        if (!try self.expect(.open_bracket)) {
            const err = errors.ExpectedTokenError {
                .expected = .open_bracket,
                .span = .init(self.previous_token.span.end, self.file),
                .help = "match expressions must always open with a bracket",
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            _ = errid;
        }
        var match_arms: std.ArrayList(AstNodeId) = .empty;
        while (!self.is_next(.close_bracket)) {
            const pattern = try self.type_expression();
            if (!try self.expect(.fat_arrow)) {
                const err = errors.ExpectedTokenError {
                    .expected = .fat_arrow,
                    .span = .init(self.previous_token.span.end, self.file),
                };
                const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                _ = errid;
            }
            var captures: std.ArrayList(Ast.Ident) = .empty;
            if (self.next_if(.pipe)) |_| {
                while (!self.is_next(.pipe)) {
                    const ident = try self.expect_ret(.ident);
                    if (ident == null) {
                        //Expected identifier
                        return error.ParserError;
                    }
                    try captures.append(self.allocator, .{ .span = .make(ident.?.span) });
                }
                _ = self.expect(.pipe) catch unreachable;
            }
            const block = try self.expression_optional_block();

            if (!self.is_next(.comma) and !self.is_next(.close_bracket)) {
                const err = errors.ExpectedTokenError {
                    .expected = .comma,
                    .span = .init(self.previous_token.span.end, self.file)
                };

                const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                _ = errid;
            }
            _ = self.next_if(.comma);

            const match_arm_node = Ast.MatchArm {
                .captures = if (captures.items.len == 0) null else try captures.toOwnedSlice(self.allocator),
                .pattern = pattern,
                .block = block,
            };

            span.merge(.init(self.lexer.index, self.file));
            const match_arm_nodeid = try self.builder.add_node(.match_arm, span, match_arm_node);

            try match_arms.append(self.allocator, match_arm_nodeid);
        }
        _ = self.expect(.close_bracket) catch unreachable;

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
    var left = try self.expression_unary();
    while (self.is_next_one_of(.{.star, .slash, .percent})) {
        const tok = self.next() catch unreachable;
        const right = try self.expression_unary();
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
    var expr = try self.expression_sequence();

    while (self.is_next_one_of(.{.open_paren, .open_square, .dot, .question, .question2})) {
        if (self.next_if(.open_paren)) |_| {
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
                    .id = if (ident) |id| .{ .span = .make(id.span) } else null,
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
            expr = nodeid;
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
            expr = nodeid;
        }

        if (self.next_if(.dot)) |_| {
            const ident = if (self.is_next_one_of(.{ .ident, .int_literal })) self.next() catch unreachable else null;
            if (ident == null) {
                const err = errors.ExpectedTokensError {
                    .expected = &[_]lex.Tag{.ident, .int_literal},
                    .span = .init(self.previous_token.span.end, self.file),
                };
                const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                return errid;
            }

            const node = Ast.AccessOperator {
                .left = expr,
                .right = ident.?,
            };

            span.merge(.init(self.lexer.index, self.file));
            const nodeid = try self.builder.add_node(.access_operator, span, node);
            expr = nodeid;
        }

        if (self.is_next_one_of(.{.question, .question2})) {
            const tok = self.next() catch unreachable;

            expr = try self.builder.add_node(.unwrap, span, Ast.Unwrap { .expr = expr });

            if (tok.tag == .question2) {
                expr = try self.builder.add_node(.unwrap, span, Ast.Unwrap { .expr = expr });
            }
        }
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
        if (self.next_if(.ident)) |id| {
            const node = Ast.Terminal {
                .termtype = .{ .symbol = .{ .span = .make_a(id.span) }},
            };
            span.merge(.init(self.lexer.index, self.file));
            const nodeid = try self.builder.add_node(.terminal, span, node);
            return nodeid;
        }
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
            const expr = try self.expression();
            const comma = self.next_if(.comma);
            if (comma == null and !self.is_next(.close_bracket)) {
                const err = errors.ExpectedTokenError {
                    .expected = .close_bracket,
                    .span = .init(self.previous_token.span.end, self.file),
                };

                const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                const errnode = try self.builder.add_node(.poison, span, Ast.Poison { .error_id = errid });
                return errnode;
            }
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
        var expr: AstNodeId = undefined;
        if (!self.is_next(.close_paren)) {
            expr = try self.expression();
        }
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

    if (self.is_next(.back_slash)) {
        return try self.expression_lambda();
    }

    if (self.is_next(.open_bracket)) {
        return try self.expression_block();
    }
    return try self.expression_literal();
}


fn expression_lambda(self: *@This()) !AstNodeId {
    var span: common.Span = .init(self.lexer.index, self.file);

    if (!try self.expect(.back_slash)) {
        //This function should never be called without checking if it should
        unreachable;
    }
    var params: std.ArrayList(Ast.LambdaParam) = .empty;
    var ret_ty: ?AstNodeId = null;
    if (self.is_next_one_of(.{.dollar, .ident})) {
        const tok = self.next() catch unreachable;
        const is_generic = tok.tag == .dollar;
        var ident = tok;
        if (is_generic) {
            const id = try self.expect_ret(.ident);
            if (id == null) {
                const err = errors.ExpectedTokenError {
                    .expected = .ident,
                    .span = .init(self.previous_token.span.end, self.file)
                };
                const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                return errid;
            }
            ident = id.?;
        }
        
        const param = Ast.LambdaParam {
            .ident = .{ .span = .make_a(ident.span) },
            .mod = null,
            .ty = null,
        };
        try params.append(self.allocator, param);
    } else {
        if (!try self.expect(.open_paren)) {
            const err = errors.ExpectedTokenError {
                .expected = .open_paren,
                .span = .init(self.previous_token.span.end, self.file),
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            _ = errid;
        }

        _ = self.next_if(.open_paren);

        while (!self.is_next(.close_paren)) {
            const is_generic = try self.expect(.dollar);
            var bind_mod: ?Ast.BindingModifier = null;
            if (!is_generic) {
                bind_mod = try self.binding_modifier();
            }
            const id = try self.expect_ret(.ident);
            if (id == null) {
                const err = errors.ExpectedTokenError {
                    .expected = .ident,
                    .span = .init(self.previous_token.span.end, self.file)
                };
                const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                return errid;
            }
            var type_expr: ?AstNodeId = null;
            if (self.next_if(.colon)) |_| {
                type_expr = try self.type_expression();
            }

            if (!self.is_next(.comma) and !self.is_next(.close_paren)) {
                const err = errors.ExpectedTokenError {
                    .expected = .comma,
                    .span = .init(self.previous_token.span.end, self.file),
                };
                const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                _ = errid;
            }
            _ = self.next_if(.comma);

            const param = Ast.LambdaParam {
                .ident = .{ .span = .make_a(id.?.span) },
                .mod = bind_mod,
                .ty = type_expr,
            };

            try params.append(self.allocator, param);
        }
        _ = try self.expect(.close_paren);
        if (!self.is_next(.fat_arrow)) {
            ret_ty = try self.type_expression();
        }
    }

    if (!try self.expect(.fat_arrow)) {
        const err = errors.ExpectedTokenError {
            .expected = .fat_arrow,
            .span = .init(self.previous_token.span.end, self.file),
        };
        const errid = try self.context.session.push(try err.get_error_type(self.allocator));
        _ = errid;
    }

    const body = try self.expression_optional_block();

    const node = Ast.Lambda {
        .params = try params.toOwnedSlice(self.allocator),
        .ret_ty = ret_ty,
        .expr = body,
    };
    span.merge(.init(self.lexer.index, self.file));
    const nodeid = try self.builder.add_node(.lambda, span, node);
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
        .ident,
    })) {
        const tok = self.next() catch unreachable;
        const node = Ast.Terminal {
            .termtype = .{
                .value = tok,
            },
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
            .termtype = .{
                .symbol = .{ .span = .make(ident.?.span) },
            }
        };

        span.merge(.init(self.lexer.index, self.file));
        const nodeid = try self.builder.add_node(.terminal, span, node);
        return nodeid;
    }

    const err = errors.UnexpectedTokenError {
        .found = try self.next(),
    };

    const errid = try self.context.session.push(try err.get_error_type(self.allocator));
    span.merge(.init(self.lexer.index, self.file));
    const out = try self.builder.add_node(.poison, self.previous_token.span, Ast.Poison { .error_id = errid });
    return out;
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
    })) {
        switch(self.peek().?.tag) {
            .keyword_let => return try self.let_binding(),
            .keyword_fn => return try self.function_declaration(),
            else => unreachable,
        }
    }

    if (self.is_next(.keyword_return)) {
        const expr = try self.expression_return();
        if (self.next_if(.semicolon) == null) {
            const err = errors.ExpectedTokenError {
                .expected = .semicolon,
                .span = .init(self.previous_token.span.end, self.file),
            };
            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            _ = errid;
        }
        return expr;
    }

    const left = try self.expression();

    if (self.next_if(.semicolon)) |_| {
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
        if (!try self.expect(.semicolon)) {
            const err = errors.ExpectedTokenError {
                .expected = .semicolon,
                .span = .init(self.previous_token.span.end, self.file),
            };

            const errid = try self.context.session.push(try err.get_error_type(self.allocator));
            _ = errid;
        }
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

    fn try_parse_attributes(self: *@This()) !?[]Ast.Attribute {

        var attribs: std.ArrayList(Ast.Attribute) = .empty;
        while (self.is_next(.at)) {
            _ = try self.expect(.at);
            const id = try self.expect_ret(.ident);
            if (id == null) {
                const err = errors.ExpectedTokenError {
                    .expected = .ident,
                    .span = .init(self.previous_token.span.end, self.file),
                };

                const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                _ = errid;
                // Try to resync the parser if there is a parameter list
                // Probably need to imrove this
                if (self.next_if(.open_paren)) |_| {
                    while (!self.is_next(.close_paren)) {
                        _ = try self.next();
                    }
                }

                return null;
            }
            var args: std.ArrayList(Ast.AstNodeId) = .empty;
            if (self.next_if(.open_paren)) |_| {
                while (!self.is_next(.close_paren)) {
                    const expr = try self.expression();
                    if (!self.is_next(.comma) and !self.is_next(.close_paren)) {
                        const err = errors.ExpectedTokenError {
                            .expected = .comma,
                            .span = .init(self.previous_token.span.end, self.file),
                        };

                        const errid = try self.context.session.push(try err.get_error_type(self.allocator));
                        _ = errid;
                    }
                    _ = self.next_if(.comma);

                    try args.append(self.allocator, expr);
                }
                _ = try self.expect(.close_paren);
            }

            const attrib = Ast.Attribute {
                .args = if (args.items.len == 0) null else try args.toOwnedSlice(self.allocator),
                .id = .{ .span = .make_a(id.?.span) },
            };

            try attribs.append(self.allocator, attrib);
        }


        return if (attribs.items.len == 0) null else try attribs.toOwnedSlice(self.allocator);
    }
