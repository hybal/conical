const std = @import("std");
const Ast = @import("Ast.zig");
const types = @import("types.zig");
const Hir = @import("Hir.zig");
const diag = @import("diag.zig");
const mem = @import("mem.zig");


allocator: std.mem.Allocator,
sym_tab: std.ArrayList(types.SymbolTable),
def_table: std.StringHashMap(Hir.DefId),
hir_table: Hir.HirInfoTable,
type_tbl: types.TypeTbl,
session: *diag.Session,
in_function: bool = false,
in_assignment: bool = false,
at_global_scope: bool = true,
//expected_type: ?Ast.TypeId = null,
current_scope: usize = 0,
source: []const u8,


fn enter_scope(self: *@This()) !void {
    var children: std.ArrayList(usize) = undefined;
    const current_children = self.sym_tab.items[self.current_scope].children;
    if (current_children) |ccsn| {
        children = .fromOwnedSlice(self.allocator, ccsn);
    } else {
        children = .init(self.allocator);
    }
    const child_index = self.sym_tab.items.len;
    try children.append(child_index);
    self.sym_tab.items[self.current_scope].children = try children.toOwnedSlice();
    const symtab = types.SymbolTable {
        .symbol_map = .init(self.allocator),
        .parent = self.current_scope,
        .children = null,
    };
    try self.sym_tab.append(symtab);
    self.current_scope = child_index;
}

fn leave_scope(self: *@This()) void {
    if (self.sym_tab.items[self.current_scope].parent) |parent| {
        self.current_scope = parent;
    }
}

fn add_symbol(self: *@This(), name: []const u8, symbol: types.Symbol) !void {
    const defid = std.hash.Fnv1a_64.hash(name);
    _ = try self.def_table.getOrPutValue(name, defid);
    if (self.sym_tab.items[self.current_scope].symbol_map.contains(defid)) {
        return error.SymbolShadow;
    }
    try self.sym_tab.items[self.current_scope].symbol_map.put(defid, symbol);
}

fn get_symbol(self: *@This(), name: []const u8) ?types.Symbol {
    const defid = self.def_table.get(name);
    if (defid) |dfid| {
        var current_scope = self.sym_tab.items[self.current_scope];
        var exit = false;
        while (!exit) {
            if (current_scope.symbol_map.get(dfid)) |outsym| {
                return outsym;
            }
            if (current_scope.parent) |parent| {
                current_scope = self.sym_tab.items[parent];
            } else {
                exit = true;
            }
        }
        return null;
    }
    return null;
}


pub fn init_context(session: *diag.Session, source: []const u8, type_tbl: types.TypeTbl, allocator: std.mem.Allocator) !@This() {
    var sym_tab: std.ArrayList(types.SymbolTable) = .init(allocator);
    try sym_tab.append(.{
        .symbol_map = .init(allocator),
        .children = null,
        .parent = null,
    });
    const self: @This() = .{
        .allocator = allocator,
        .sym_tab = sym_tab,
        .type_tbl = type_tbl,
        .def_table = .init(allocator),
        .session = session,
        .source = source,
        .hir_table = .init(allocator),
    };
    return self;
}

pub fn lower(self: *@This(), ast: []*Ast.Ast) ![]Hir.Hir {
    var out: std.ArrayList(Hir.Hir) = .init(self.allocator);
    try self.resolve_global_symbols(ast);
    for (ast) |tree| {
        try self.resolve_local_symbols(tree);
        try out.append(try self.lower_single(tree));
    }
    return try out.toOwnedSlice();

}

fn resolve_global_symbols(self: *@This(), trees: []*Ast.Ast) !void {
    for (trees) |ast| {
        switch (ast.node) {
            .var_decl => |decl| {
                self.add_symbol(decl.ident.span.get_string(self.source), .{
                    .tyid = decl.ty,
                    .name = decl.ident.value,
                    .node = ast,
                    .scope = .Global,
                }) catch {
                    return error.VariableShadowsPreviousDecleration;
                };
            },
            .fn_decl => |decl| {
                const fnctypeid = try decl.hash(&self.type_tbl, self.allocator);
                self.add_symbol(decl.ident.value, .{
                    .tyid = fnctypeid,
                    .name = decl.ident.value,
                    .node = ast,
                    .scope = .Global
                }) catch {
                    return error.FunctionShadowsPreviousDecleration;
                };
            },
            .type_decl => |decl| {
                self.add_symbol(decl.ident.value, .{
                    .tyid = decl.ty,
                    .name = decl.ident.value,
                    .node = ast,
                    .scope = .Global,
                }) catch {
                    return error.TypeShadowsPreviousDecleration;
                };
            },
            else => unreachable,
        }
    }
}

fn resolve_local_symbols(self: *@This(), ast: *Ast.Ast) !void {
    switch (ast.node) {
        .var_decl => |decl| {
            if (!self.at_global_scope) {
                self.add_symbol(decl.ident.value, .{
                    .name = decl.ident.value,
                    .node = ast,
                    .tyid = decl.ty,
                }) catch {
                    return error.VariableShadowsPreviousDecleration;
                };
            } 
            if (decl.initialize) |init| {
                try self.resolve_local_symbols(init);
            }
        },
        .fn_decl => |decl| {
            if (!self.at_global_scope) {
                self.add_symbol(decl.ident.value, .{
                    .name = decl.ident.value,
                    .node = ast,
                    .tyid = try decl.hash(&self.type_tbl, self.allocator)
                }) catch {
                    return error.FunctionShadowsPreviousDecleration;
                };
            }
            if (decl.body) |body| {
                const saved_at_global_scope = self.at_global_scope;
                self.at_global_scope = false;
                const saved_in_function = self.in_function;
                self.in_function = true;
                try self.enter_scope();
                for (decl.params, 0..) |param_id, i| {
                    self.add_symbol(param_id.value, .{
                        .name = param_id.value,
                        .node = ast,
                        .tyid = decl.param_types[i],
                        .scope = .LocalEscapes,
                    }) catch |err| {
                        switch (err) {
                            error.SymbolShadow => {},
                            else => return err,
                        }
                    };
                }
                for (body.node.block.exprs) |expr| {
                    try self.resolve_local_symbols(expr);
                }
                self.at_global_scope = saved_at_global_scope;
                self.in_function = saved_in_function;
                self.leave_scope();
            }
        },
        .type_decl => |decl| {
            if (!self.at_global_scope) {
                self.add_symbol(decl.ident.value, .{
                    .name = decl.ident.value,
                    .node = ast,
                    .tyid = decl.ty
                }) catch {
                    return error.TypeShadowsPreviousDecleration;
                };
            }
        },
        .binary_expr => |expr| {
            try self.resolve_local_symbols(expr.left);
            try self.resolve_local_symbols(expr.right);
        },
        .unary_expr => |expr| {
            try self.resolve_local_symbols(expr.expr);
        },
        .assignment => |expr| {
            try self.resolve_local_symbols(expr.lvalue);
            try self.resolve_local_symbols(expr.expr);
        },
        .block => |exprs| {

            try self.enter_scope();
            for (exprs.exprs) |expr| {
                try self.resolve_local_symbols(expr);
            }
            self.leave_scope();
        },
        .cast => |expr| {
            try self.resolve_local_symbols(expr.expr);
        },
        .fn_call => |expr| {
            try self.resolve_local_symbols(expr.left);
            for (expr.params) |param| {
                try self.resolve_local_symbols(param);
            }
        },
        .return_stmt => |expr| {
            try self.resolve_local_symbols(expr);
        },
        .terminated => |expr| try self.resolve_local_symbols(expr),
        .terminal => |expr| {
            if (expr.tag == .ident) {
                if (self.get_symbol(expr.span.get_string(self.source)) == null) {
                    try self.session.emit(.Error, expr.span, "Unknown Identifier");
                    return error.UnknownIdentifier;
                }
            }
        },
        .if_stmt => |expr| {
            try self.resolve_local_symbols(expr.condition);
            try self.resolve_local_symbols(expr.block);
            if (expr.else_block) |else_block| {
                try self.resolve_local_symbols(else_block);
            }
        },
        .access_operator => |expr| {
            try self.resolve_local_symbols(expr.left);
            if (self.get_symbol(expr.right.value) == null) {
                try self.session.emit(.Error, expr.right.span, "Unknown Indentifier");
                return error.UnknownIdentifier;
            }
        },
        .while_loop => |expr| {
            try self.resolve_local_symbols(expr.condition);
            try self.resolve_local_symbols(expr.block);
        },
        .type_cons => |cons| {
            const ty = self.type_tbl.get(cons.ty).?;
            if (ty.base_type == .user) {
                if (self.get_symbol(ty.base_type.user.value) == null) {
                    try self.session.emit(.Error, ty.base_type.user.span, "Unknown Identifier");
                    return error.UnknownIdentifier;
                }
            }
        },
        else => {
            std.debug.print("Unhandled case: {s}\n", .{@tagName(ast.node)});
            unreachable;
        },

    }
}
fn parse_int_literal(self: *@This(), span: types.Span) !u128 {
    const literal_string = span.get_string(self.source);
    return std.fmt.parseInt(u128, literal_string, 0);
}

fn parse_float_literal(self: *@This(), span: types.Span) !f64 {
    const literal_string = span.get_string(self.source);
    return std.fmt.parseFloat(f64, literal_string);
}
fn parse_escape(str: []const u8) !i32 {
    switch (str[0]) {
        'n' => return '\n',
        'r' => return '\r',
        't' => return '\t',
        'b' => return 0x8,
        'f' => return 0xC,
        'v' => return 0xB,
        '\'' => return '\'',
        '\"' => return '\"',
        '0' => return 0x0,
        'x', 'u', 'U' => {
            const num = str[1..];
            return std.fmt.parseInt(i32, num, 16);
        },
        else => {
            return error.InvalidEscapeSequence;
        }
    }
}
fn parseHexEscape(comptime T: type, slice: []const u8) !T {
    return std.fmt.parseInt(T, slice, 16) catch return error.InvalidHexEscape;
}
fn unescape_string(self: *@This(), input: []const u8) ![]u8 {
    var list = std.ArrayList(u8).init(self.allocator);
    defer list.deinit();

    var i: usize = 0;
    while (i < input.len) : (i += 1){
        if (input[i] == '\\' and i + 1 < input.len) {
            i += 1;
            switch (input[i]) {
                'n' => try list.append('\n'),
                'r' => try list.append('\r'),
                't' => try list.append('\t'),
                'b' => try list.append(0x8),
                'f' => try list.append(0xC),
                'v' => try list.append(0xB),
                '\\' => try list.append('\\'),
                '"' => try list.append('"'),
                '\'' => try list.append('\''),
                '0' => try list.append(0),
                'x' => {
                    // \xHH (2 hex digits)
                    if (i + 2 >= input.len) return error.InvalidHexEscape;
                    const hex = input[i+1 .. i+3];
                    const value = try parseHexEscape(u8, hex);
                    try list.append(value);
                    i += 2;
                },

                'u' => {
                    // \uHHHH (4 hex digits)
                    if (i + 4 >= input.len) return error.InvalidUnicodeEscape;
                    const hex = input[i+1 .. i+5];
                    const codepoint = try parseHexEscape(u21, hex);
                    var buf: [4]u8 = undefined;
                    const len = std.unicode.utf8Encode(codepoint, &buf) catch return error.InvalidCodepoint;
                    try list.appendSlice(buf[0..len]);
                    i += 4;
                },

                'U' => {
                    // \UHHHHHHHH (8 hex digits)
                    if (i + 8 >= input.len) return error.InvalidUnicodeEscape;
                    const hex = input[i+1 .. i+9];
                    const codepoint = try parseHexEscape(u21, hex);
                    var buf: [4]u8 = undefined;
                    const len = std.unicode.utf8Encode(codepoint, &buf) catch return error.InvalidCodepoint;
                    try list.appendSlice(buf[0..len]);
                    i += 8;
                },                
                else => unreachable,
            }
        } else {
            try list.append(input[i]);
        }
    }

    return list.toOwnedSlice();
}

fn parse_char_literal(self: *@This(), span: types.Span) !i32 {
    var local_span: types.Span = .{.start = span.start, .end = span.end};
    local_span.start += 1;
    local_span.end -= 1;
    const char_string = local_span.get_string(self.source);
    if (char_string[0] == '\\') {
        return parse_escape(char_string[0..]);
    }
    return char_string[0];

}
fn pow(base: comptime_int, power: comptime_int) comptime_int {
    var out = 1;
    for (0..power) |_| {
        out = out * base;
    }
    return out;
}
fn get_int_type(value: u128, signed: bool) Ast.PrimitiveType {
    return switch (value) {
        0...pow(2, 8) - 1 => if (signed) .I8 else .U8,
        pow(2, 8)...pow(2, 16) - 1 => if (signed) .I16 else .U16,
        pow(2, 16)...pow(2, 32) - 1 => if (signed) .I32 else .U32,
        pow(2, 32)...pow(2, 64) - 1 => if (signed) .I64 else .U64,
        pow(2, 64)...pow(2, 128) - 1 => if (signed) .I128 else .U128
    };
}

fn get_float_type(value: f64) Ast.PrimitiveType {
    const conv = @as(f32, @floatCast(value));
    return if (@as(f64, conv) == value) .F32 else .F64;
}

fn lower_single(self: *@This(), ast: *Ast.Ast) !Hir.Hir {
    var out_node: ?Hir.HirNode = null;
    switch (ast.node) {
        .terminal => |expr| {
            var terminal: Hir.Terminal = Hir.Terminal.unit;
            switch (expr.tag) {
                .ident => {
                    const id = expr.span.get_string(self.source);
                    const defid = self.def_table.get(id).?;
                    terminal = .{.identifier = defid };

                },
                .int_literal => {
                    const value = try self.parse_int_literal(expr.span);
                    terminal = .{ 
                        .integer_literal = value
                    };
                },
                .float_literal => {
                    const value = try self.parse_float_literal(expr.span);
                    terminal = .{
                        .float_literal = value,
                    };
                },
                .char_literal => {
                    //NOTE: will probably have to adjust for quotes
                    terminal = .{
                        .char_literal = 
                            try self.parse_char_literal(expr.span)
                        };
                    },
                    .string_literal => {
                        //NOTE: will probably have to adjust for quotes
                        terminal = .{
                            .string_literal =
                                try self.unescape_string(expr.span.get_string(self.source))
                            };
                        },
                        .raw_string_literal => {
                            //NOTE: will probably have to adjust for quotes
                            terminal = .{
                                .string_literal = expr.span.get_string(self.source),
                            };
                        },
                        .keyword_true, .keyword_false => {
                            terminal = .{
                                .bool_literal = if (expr.tag == .keyword_true) true else false,
                            };
                        },
                        else => unreachable
            }
            out_node = .{ .inline_expr = .{ .terminal = try mem.createWith(self.allocator, terminal)}};
        },
        .unary_expr => |expr| {
            const expr_node = try self.lower_single(expr.expr);
            var op: Hir.UnaryOp = undefined;
            switch (expr.op.tag) {
                .minus => {
                    op = .Minus;
                },
                .tilde => {
                    op = .BinNot;
                },
                .bang => {
                    op = .Not;
                },
                .amp, .amp2 => {
                    op = .Ref;

                },
                .star => {
                    op = .DeRef;
                },
                else => unreachable,
            }
            const node: Hir.UnaryExpr = .{ .op = op, .expr = expr_node };
            out_node = .{ .inline_expr = .{ .unary_expr = try mem.createWith(self.allocator, node)}};
        },
        .binary_expr => |expr| {
            const left = try self.lower_single(expr.left);
            var node: Hir.BinaryExpr = undefined;
            switch (expr.op.tag) {
                .plus, .minus, .slash, .star, .caret, .percent,
                .pipe, .amp, .shl, .shr => {
                    const right = try self.lower_single(expr.right);
                    const op: Hir.BinaryOp = switch (expr.op.tag) {
                        .plus => .Add,
                        .minus => .Sub,
                        .slash => .Div,
                        .star => .Mul,
                        .percent => .Mod,
                        .pipe => .BinOr,
                        .amp => .BinAnd,
                        .shl => .Shl,
                        .shr => .Shr,
                        .caret => .BinXor,
                        else => unreachable,
                    };
                    node = .{
                        .lhs = left,
                        .rhs = right,
                        .op = op
                    };
                },
                .eq2, .noteq, .lt, .lteq, .gt, .gteq => {
                    const right = try self.lower_single(expr.right);
                    const op: Hir.BinaryOp = switch(expr.op.tag) {
                        .eq2 => .Eq,
                        .noteq => .NotEq,
                        .lt => .Lt,
                        .lteq => .LtEq,
                        .gt => .Gt,
                        .gteq => .GtEq,
                        else => unreachable
                    };
                    node = .{
                        .lhs = left,
                        .rhs = right,
                        .op = op
                    };
                },
                .amp2, .pipe2 => {
                    const right = try self.lower_single(expr.right);
                    const op: Hir.BinaryOp = switch (expr.op.tag) {
                        .amp2 => .LAnd,
                        .pipe2 => .LOr,
                        else => unreachable,
                    };
                    node = .{
                        .lhs = left,
                        .rhs = right,
                        .op = op
                    };
                },
                else => unreachable,
            }
            out_node = .{ .inline_expr = .{
                .binary_expr = try mem.createWith(self.allocator, node)
            }};
        },
        .assignment => |expr| {
            const left = try self.lower_single(expr.lvalue);
            const saved_in_assignment = self.in_assignment;
            self.in_assignment = true;
            const right = try self.lower_single(expr.expr);
            self.in_assignment = saved_in_assignment;
            var assignment_node: Hir.Assignment = .{
                .expr = right,
                .lvalue = left,
            };
            if (expr.op.tag != .eq) {
                const op: Hir.BinaryOp = switch (expr.op.tag) {
                    .pluseq => .Add,
                    .minuseq => .Sub,
                    .slasheq => .Div,
                    .stareq => .Mul,
                    .careteq => .BinXor,
                    .percenteq => .Mod,
                    .pipeeq => .BinOr,
                    .ampeq => .BinAnd,
                    .shleq => .Shl,
                    .shreq => .Shr,
                    else => unreachable,
                };
                const desugered_expr: Hir.BinaryExpr = .{
                    .lhs = left,
                    .rhs = right,
                    .op = op
                };
                assignment_node.expr = try Hir.Hir.create(.{ .inline_expr = .{ 
                    .binary_expr = try mem.createWith(self.allocator, desugered_expr)
                }}, ast.span, &self.hir_table);
            }
            out_node = .{ .top_level = .{ 
                .assignment = try mem.createWith(self.allocator, assignment_node)}};
            },
            .cast => |expr| {
                const left = try self.lower_single(expr.expr);
                const out: Hir.Cast = .{
                    .expr = left,
                    .tyid = expr.ty
                };
                out_node = .{ .inline_expr = .{
                    .cast = try mem.createWith(self.allocator, out)
                }};
            },
            .block => |expr| {
                var out_block: std.ArrayList(Hir.Hir) = .init(self.allocator);
                for (0..expr.exprs.len - 1) |i| {
                    try out_block.append(try self.lower_single(expr.exprs[i]));
                }

                const hir = try self.lower_single(expr.exprs[expr.exprs.len - 1]);
                if (self.in_function and expr.exprs[expr.exprs.len - 1].node != .return_stmt) {
                    const return_node: Hir.Return = .{
                        .expr = hir,
                    };
                    try out_block.append(try Hir.Hir.create(.{ .top_level = .{
                        .return_stmt = try mem.createWith(self.allocator, return_node),
                    }}, ast.span, &self.hir_table));
                } else {
                    try out_block.append(hir);
                }
                out_node = .{
                    .inline_expr = .{ 
                        .block = try mem.createWith(self.allocator, Hir.Block{ .body = try out_block.toOwnedSlice()}),
                    }
                };
            },
            .fn_decl => |decl| {
                var params: std.ArrayList(struct { id: Ast.Ident, ty: Ast.TypeId }) = .init(self.allocator);
                for (decl.params, decl.param_types) |param, param_ty| {
                    try params.append(.{ .id = param, .ty = param_ty });
                }
                //TODO: Handle modifiers
                const fnc: Hir.Fn = .{
                    .id = .{ .value = decl.ident.value, .location = decl.ident.span },
                    .body = if (decl.body) |body| try self.lower_single(body) else null,
                    .return_type = decl.return_ty,
                    .parameters = @ptrCast(try params.toOwnedSlice()),
                    .is_public = true,
                    .is_extern = false,
                    .is_export = true,
                };

                out_node = .{ .top_level = .{ .func = try mem.createWith(self.allocator, fnc)} };
            },
            .type_decl => |decl| {
                const tydecl = Hir.TypeDecl {
                    .id = .{.location = decl.ident.span, .value = decl.ident.value },
                    .is_extern = false,
                    .is_pub = false,
                    .tyid = decl.ty
                };
                out_node = .{ .top_level = .{ .type_decl = try mem.createWith(self.allocator, tydecl)}};
            },
            .var_decl => |decl| {
                const out: Hir.Binding = .{
                    .id = .{ .location = decl.ident.span, .value = decl.ident.value },
                    .ty = decl.ty,
                    .is_mutable = decl.is_mut,
                    .expr = try self.lower_single(decl.initialize.?),
                    .is_extern = false,
                    .is_pub = true,
                    .is_static = false,
                    .is_export = false,
                };
                out_node = .{ .top_level = .{ .binding = try mem.createWith(self.allocator, out)}};
            },
            .return_stmt => |stmt| {
                const out: Hir.Return = .{ 
                    .expr = try self.lower_single(stmt)
                };
                out_node = .{ .top_level = .{ .return_stmt = try mem.createWith(self.allocator, out)}};
            },
            .fn_call => |expr| {
                var args = try std.ArrayList(Hir.Hir).initCapacity(self.allocator, expr.params.len);
                for (expr.params) |param| {
                    try args.append(try self.lower_single(param));
                }
                const out: Hir.FnCall = .{
                    .expr = try self.lower_single(expr.left),
                    .arguments = try args.toOwnedSlice(),
                };
                out_node = .{ .inline_expr = .{ .fn_call = try mem.createWith(self.allocator, out)}};
            },
            .type_cons => |cons| {
                var ty = self.type_tbl.get(cons.ty).?;
                if (ty.base_type == .user) {
                    ty = self.type_tbl.get(self.get_symbol(ty.base_type.user.value).?.tyid.?).?;
                }
                if (ty.base_type == .@"enum") {
                    if (cons.fields.count() > 1) {
                        try self.session.emit(.Error, ast.span, "Too many values for enum literal");
                        return error.EnumLiteral;
                    }
                    if (cons.fields.count() == 0) {
                        try self.session.emit(.Error, ast.span, "Enum literal requires at least one field");
                        return error.EnumLiteral;
                    }
                    var iter = cons.fields.iterator();
                    const field = iter.next().?;
                    const enum_cons = Hir.EnumCons {
                        .ty = ty.hash(),
                        .field = field.key_ptr.*,
                        .value = field.value_ptr.*
                    };
                    out_node = .{ .inline_expr = .{
                        .enum_cons = try mem.createWith(self.allocator, enum_cons)
                    }};
                } else if (ty.base_type == .strct) {
                    if (cons.fields.count() != ty.base_type.strct.fields.count()) {
                        try self.session.emit(.Error, ast.span, "Missing or too many struct fields");
                        return error.StructLiteral;
                    }

                    var fields = std.StringHashMap(Hir.Hir).init(self.allocator);
                    var iter = cons.fields.iterator();
                    while (iter.next()) |val| {
                        try fields.put(val.key_ptr.*, try self.lower_single(val.value_ptr.*.?));
                    }
                    const struct_cons = Hir.StructCons {
                        .fields = fields,
                        .ty = ty.hash()
                    };

                    out_node = .{.inline_expr = .{
                        .struct_cons = try mem.createWith(self.allocator, struct_cons)
                    }};
                } else {
                    try self.session.emit(.Error, ast.span, "Not a compound type");
                    return error.NonCompoundTypeCons;
                }
            },
            else => |node| {
                std.debug.print("Unhandled AST node: {any}\n", .{node});
                unreachable;
            }
    }
    if (out_node == null) {
        std.debug.print("`out_node` should not be null, case missing initializer\n", .{});
        unreachable;
    }
    const id = out_node.?.hash();
    _ = try self.hir_table.getOrPutValue(id, .{
        .adjustments = .init(self.allocator),
        .span = ast.span,
        .ty = null
    });
    const out = Hir.Hir{ 
        .node = out_node.?,
        .id = id,
    };
    return out;

}

fn get_cast(self: *@This(), expected_ty: Ast.TypeId, tree: Hir.Hir) !?Hir.Cast {
    if (tree.typeid == expected_ty) {
        return null;
    }
    const ty1 = self.type_tbl.get(expected_ty).?;
    const ty2 = self.type_tbl.get(tree.typeid.?).?;
    const out: Hir.Cast = .{
        .expr = tree,
        .tyid = expected_ty
    };
    if (ty1.base_type == .primitive 
        and ty2.base_type == .primitive
        and (ty1.modifiers == null 
            and ty2.modifiers == null)) {
        if (ty1.base_type.primitive.is_signed_int()
            and ty2.base_type.primitive.is_int()
            and (ty1.base_type.primitive.get_bits(64) 
                >= ty2.base_type.primitive.get_bits(64))) {
            return out;
        }
        if (ty1.base_type.primitive.is_unsigned_int()
            and ty2.base_type.primitive.is_unsigned_int()
            and (ty1.base_type.primitive.get_bits(64)
                >= ty2.base_type.primitive.get_bits(64))) {
            return out;
        }
        if (ty1.base_type.primitive.is_float()
            and (ty2.base_type.primitive.is_int()
                or ty2.base_type.primitive.is_float())
            and (ty1.base_type.primitive.get_bits(64)
                >= ty2.base_type.primitive.get_bits(64))) {
            return out;
        }
    }
    return error.TypeMismatch;
}
