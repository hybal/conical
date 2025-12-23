const std = @import("std");
const Ast = @import("Ast.zig");
const types = @import("types.zig");
const Hir = @import("Hir.zig");
const diag = @import("diag.zig");
const mem = @import("mem.zig");


allocator: std.mem.Allocator,
def_table: std.AutoHashMap(usize, std.StringHashMap(types.DefId)),
hir_table: Hir.HirInfoTable,
context: *types.Context,
in_function: bool = false,
in_assignment: bool = false,
at_global_scope: bool = true,
//expected_type: ?Ast.TypeId = null,
current_scope: usize = 0,
// Map of A -> B where A is the id right after the cleanup and B is the node to be destroyed
escaped_ids: std.AutoHashMap(Hir.HirId, Hir.HirId),


fn enter_new_scope(self: *@This()) !void {
    var children: std.ArrayList(usize) = undefined;
    const current_children = self.context.sym_tab.items[self.current_scope].children;
    if (current_children) |ccsn| {
        children = .fromOwnedSlice(self.allocator, ccsn);
    } else {
        children = .init(self.allocator);
    }
    const child_index = self.context.sym_tab.items.len;
    try children.append(child_index);
    self.context.sym_tab.items[self.current_scope].children = try children.toOwnedSlice();
    const symtab = types.SymbolTable {
        .symbol_map = .init(self.allocator),
        .parent = self.current_scope,
        .children = null,
    };
    try self.context.sym_tab.append(symtab);
    self.current_scope = child_index;
}

fn leave_scope(self: *@This()) void {
    if (self.context.sym_tab.items[self.current_scope].parent) |parent| {
        self.current_scope = parent;
    }
}


fn add_symbol(self: *@This(), symbol: 
    struct {tyid: ?Ast.TypeId, name: Ast.Ident, scope: types.SymbolScope = .Local}
    ) !void {
    const path = types.Path {
        .base = symbol.name,
        .module = self.context.module.?,
    };
    const defid = path.hash();
    const sym = types.Symbol {
        .name = symbol.name,
        .tyid = symbol.tyid,
        .path = path,
        .scope = symbol.scope,
    };
    if (self.context.sym_tab.items[self.current_scope].symbol_map.contains(defid)) {
        return error.SymbolShadow;
    }

    if (!self.def_table.contains(self.current_scope)) {
        try self.def_table.put(self.current_scope, .init(self.allocator));
    }
    try self.def_table.getPtr(self.current_scope).?.put(symbol.name.value, defid);
    try self.context.sym_tab.items[self.current_scope].symbol_map.put(defid, sym);
}

fn get_symbol(self: *@This(), name: []const u8) ?types.Symbol {
    var current_scope = self.context.sym_tab.items[self.current_scope];
    var current_scope_id = self.current_scope;
    var exit = false;
    while (!exit) {
        var dfid: usize = undefined;
        if (self.def_table.get(current_scope_id)) |def_table| {
            if (def_table.get(name)) |def| {
                dfid = def;
            } else {
                if (current_scope.parent) |parent| {
                    current_scope = self.context.sym_tab.items[parent];
                    current_scope_id = parent;
                } else {
                    exit = true;
                }
                continue;
            }
        }
        if (current_scope.symbol_map.get(dfid)) |outsym| {
            return outsym;
        }
        if (current_scope.parent) |parent| {
            current_scope = self.context.sym_tab.items[parent];
            current_scope_id = parent;
        } else {
            exit = true;
        }
    }
    return null;
}

fn get_defid(self: *@This(), name: []const u8) ?types.DefId {
    return self.def_table.get(self.current_scope).?.get(name);
}


pub fn init(context: *types.Context, allocator: std.mem.Allocator) !@This() {
    var sym_tab: std.ArrayList(types.SymbolTable) = .init(allocator);
    try sym_tab.append(.{
        .symbol_map = .init(allocator),
        .children = null,
        .parent = null,
    });
    context.sym_tab = sym_tab;
    const self: @This() = .{
        .allocator = allocator,
        .def_table = .init(allocator),
        .hir_table = .init(allocator),
        .escaped_ids = .init(allocator),
        .context = context,
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
                self.add_symbol(.{
                    .tyid = decl.ty,
                    .name = decl.ident,
                    .scope = .Global,
                }) catch {
                    return error.VariableShadowsPreviousDecleration;
                };
            },
            .fn_decl => |decl| {
                const fnctypeid = try decl.hash(&self.context.type_tab, self.allocator);
                self.add_symbol(.{
                    .tyid = fnctypeid,
                    .name = decl.ident,
                    .scope = .Global,
                }) catch {
                    return error.FunctionShadowsPreviousDecleration;
                };
            },
            .type_decl => |decl| {
                self.add_symbol(.{
                    .tyid = decl.ty,
                    .name = decl.ident,
                    .scope = .Global,
                }) catch {
                    return error.TypeShadowsPreviousDecleration;
                };
            },
            else => |v| {
                std.debug.print("Unimplemented: {s}\n", .{@tagName(v)});
                unreachable;
            }
        }
    }
}

fn resolve_local_symbols(self: *@This(), ast: *Ast.Ast) !void {
    ast.scope_id = self.current_scope;
    switch (ast.node) {
        .var_decl => |decl| {
            if (!self.at_global_scope) {
                try self.add_symbol(.{
                    .name = decl.ident,
                    .tyid = decl.ty,
                }); 
            } 
            if (decl.initialize) |var_init| {
                try self.resolve_local_symbols(var_init);
            }
        },
        .fn_decl => |decl| {
            if (!self.at_global_scope) {
                self.add_symbol(.{
                    .name = decl.ident,
                    .tyid = try decl.hash(&self.context.type_tab, self.allocator),
                }) catch {
                    return error.FunctionShadowsPreviousDecleration;
                };
            }
            if (decl.body) |body| {
                const saved_at_global_scope = self.at_global_scope;
                self.at_global_scope = false;
                const saved_in_function = self.in_function;
                self.in_function = true;
                try self.enter_new_scope();
                ast.node.fn_decl.body.?.scope_id = self.current_scope;
                for (decl.params, 0..) |param_id, i| {
                    self.add_symbol(.{
                        .name = param_id,
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
                self.add_symbol(.{
                    .name = decl.ident,
                    .tyid = decl.ty,
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

            try self.enter_new_scope();
            ast.scope_id = self.current_scope;
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
                unreachable;
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
                try self.context.session.emit(.Error, expr.right.span, "Unknown Indentifier");
                return error.UnknownIdentifier;
            }
        },
        .while_loop => |expr| {
            try self.resolve_local_symbols(expr.condition);
            try self.resolve_local_symbols(expr.block);
        },
        .type_cons => |cons| {
            const ty = self.context.type_tab.get(cons.ty).?;
            if (ty.base_type == .user) {
                if (self.get_symbol(ty.base_type.user.value) == null) {
                    try self.context.session.emit(.Error, ty.base_type.user.span, "Unknown Identifier");
                    return error.UnknownIdentifier;
                }
            }
        },
        .unit => {

        },
        .path => |path| {
            const last = path.parts[path.parts.len - 1];
            if (self.get_symbol(last.value) == null) {
                const err_msg = try std.fmt.allocPrint(self.allocator,
                    "Unknown Identifier: {s}", .{last.value});
                try self.context.session.emit(.Error, last.span, err_msg);
                return error.UnkownIdentifier;
            }
        },
        else => {
            std.debug.print("Unhandled case: {s}\n", .{@tagName(ast.node)});
            unreachable;
        },

    }
}
fn parse_int_literal(self: *@This(), span: types.Span) !u128 {
    const literal_string = span.get_string(self.context.source);
    return std.fmt.parseInt(u128, literal_string, 0);
}

fn parse_float_literal(self: *@This(), span: types.Span) !f64 {
    const literal_string = span.get_string(self.context.source);
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
    const char_string = local_span.get_string(self.context.source);
    if (char_string[0] == '\\') {
        return parse_escape(char_string[0..]);
    }
    return char_string[0];

}


fn lower_single(self: *@This(), ast: *Ast.Ast) !Hir.Hir {
    var out_node: ?Hir.HirNode = null;
    switch (ast.node) {
        .terminal => |expr| {
            var terminal: Hir.Terminal = Hir.Terminal.unit;
            switch (expr.tag) {
                .ident => {
                    unreachable;
//                    const id = expr.span.get_string(self.context.source);
//                    const defid = self.def_table.get(id).?;
//                    terminal = .{ .path = defid };

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
                        const val = try self.unescape_string(expr.span.get_string(self.context.source));
                        const range_start: usize = if (val.len == 0) 0 else 1;
                        const range_end: usize = if(val.len == 0) 0 else val.len - 1;

                        terminal = .{ .string_literal = 
                            val[range_start..range_end]
                        };
                    },
                    .raw_string_literal => {
                        //NOTE: will probably have to adjust for quotes
                        terminal = .{
                            .string_literal = expr.span.get_string(self.context.source),
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
                }}, self.current_scope, ast.span, &self.hir_table);
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
                if (self.in_function 
                    and expr.exprs[expr.exprs.len - 1].node != .return_stmt
                    and expr.exprs[expr.exprs.len - 1].node != .terminated) {
                    const return_node: Hir.Return = .{
                        .expr = hir,
                    };
                    try out_block.append(try Hir.Hir.create(.{ .top_level = .{
                        .return_stmt = try mem.createWith(self.allocator, return_node),
                    }}, self.current_scope, ast.span, &self.hir_table));
                } else {
                    try out_block.append(hir);
                }
                const blck = Hir.Block {
                    .body = try out_block.toOwnedSlice(),
                    .scope = self.current_scope,
                };
                out_node = .{
                    .inline_expr = .{ 
                        .block = try mem.createWith(self.allocator, blck),
                    }
                };
            },
            .fn_decl => |decl| {
                var params: std.ArrayList(struct { id: Ast.Ident, ty: Ast.TypeId }) = .init(self.allocator);
                for (decl.params, decl.param_types) |param, param_ty| {
                    try params.append(.{ .id = param, .ty = param_ty });
                }
                const body = if (decl.body) |body| blk: {
                    const saved_scope = self.current_scope;
                    self.current_scope = body.scope_id.?;
                    const out = try self.lower_single(body);
                    self.current_scope = saved_scope;
                    break :blk out;
                } else null;

                var is_public = false;
                var is_extern = false;
                var is_export = false;
                if (decl.decl_mod) |mod| {
                    switch (mod) {
                        .Pub => is_public = true,
                        .Extern => is_extern = true,
                        .Export => is_export = true,
                        .PubExtern => {
                            is_public = true;
                            is_extern = true;
                        },
                        .PubExport => {
                            is_public = true;
                            is_export = true;
                        }
                    }
                }
                var fnc_ty: Ast.Type = .{
                    .base_type = .{
                        .func = .{
                            .args = decl.param_types,
                            .ret = decl.return_ty,
                        }
                    },
                    .modifiers = null,
                    .chash = null,
                };

                const fnc_ty_hash = fnc_ty.hash();
                fnc_ty.chash = fnc_ty_hash;
                try self.context.type_tab.put(fnc_ty_hash, fnc_ty);
                const fnc: Hir.Fn = .{
                    .id = self.get_defid(decl.ident.value).?,
                    .body = body,
                    .return_type = decl.return_ty,
                    .ty = fnc_ty_hash,
                    .parameters = @ptrCast(try params.toOwnedSlice()),
                    .is_public = is_public,
                    .is_extern = is_extern,
                    .is_export = is_export,
                };

                out_node = .{ .top_level = .{ .func = try mem.createWith(self.allocator, fnc)} };
            },
            .type_decl => |decl| {
                const tydecl = Hir.TypeDecl {
                    .id = self.get_defid(decl.ident.value).?,
                    .is_extern = false,
                    .is_pub = false,
                    .tyid = decl.ty
                };
                out_node = .{ .top_level = .{ .type_decl = try mem.createWith(self.allocator, tydecl)}};
            },
            .var_decl => |decl| {
                const out: Hir.Binding = .{
                    .id = self.get_defid(decl.ident.value).?,
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
                const left = try self.lower_single(expr.left);
                const out: Hir.FnCall = .{
                    .expr = left,
                    .arguments = try args.toOwnedSlice(),
                };
                out_node = .{ .inline_expr = .{ .fn_call = try mem.createWith(self.allocator, out)}};
            },
            .type_cons => |cons| {
                var ty = self.context.type_tab.get(cons.ty).?;
                if (ty.base_type == .user) {
                    ty = self.context.type_tab.get(self.get_symbol(ty.base_type.user.value).?.tyid.?).?;
                }
                if (ty.base_type == .@"enum") {
                    if (cons.fields.count() > 1) {
                        try self.context.session.emit(.Error, ast.span, "Too many values for enum literal");
                        return error.EnumLiteral;
                    }
                    if (cons.fields.count() == 0) {
                        try self.context.session.emit(.Error, ast.span, "Enum literal requires at least one field");
                        return error.EnumLiteral;
                    }
                    var iter = cons.fields.iterator();
                    const field = iter.next().?;
                    const enum_cons = Hir.EnumCons {
                        .ty = ty.hash(),
                        .field = field.key_ptr.*,
                        .value = if (field.value_ptr.*) |value| try self.lower_single(value) else null
                    };
                    out_node = .{ .inline_expr = .{
                        .enum_cons = try mem.createWith(self.allocator, enum_cons)
                    }};
                } else if (ty.base_type == .strct) {
                    if (cons.fields.count() != ty.base_type.strct.fields.count()) {
                        try self.context.session.emit(.Error, ast.span, "Missing or too many struct fields");
                        return error.StructLiteral;
                    }

                    var fields = std.StringHashMap(Hir.Hir).init(self.allocator);
                    var iter = cons.fields.iterator();
                    while (iter.next()) |val| {
                        const field_init = try self.lower_single(val.value_ptr.*.?);
                        try fields.put(val.key_ptr.*, field_init);
                    }
                    _ = try self.context.type_tab.getOrPutValue(ty.hash(), ty);
                    const struct_cons = Hir.StructCons {
                        .fields = fields,
                        .ty = ty.hash()
                    };

                    out_node = .{ .inline_expr = .{
                        .struct_cons = try mem.createWith(self.allocator, struct_cons)
                    }};
                } else {
                    try self.context.session.emit(.Error, ast.span, "Not a compound type");
                    return error.NonCompoundTypeCons;
                }
            },
            .terminated => |term| {
                out_node = .{ .top_level = .{
                    .terminated = try mem.createWith(self.allocator, try self.lower_single(term)),
                }};
            },
            .unit => {
                const terminal: Hir.Terminal = .unit;
                out_node = .{ .inline_expr = .{
                    .terminal = try mem.createWith(self.allocator, terminal)
                }};
            },
            .path => |pth| {
                //TODO: add module resolution
                if (pth.parts.len == 0) {
                    std.debug.print("Parts is zero\n", .{});
                }
                const pathid = self.get_symbol(pth.parts[pth.parts.len - 1].value).?.path.hash();
                const out = Hir.Terminal{
                    .path = pathid,
                };
                out_node = .{ .inline_expr = .{ .terminal = try mem.createWith(self.allocator, out) } };
            },
            .if_stmt => |stmt| {
                const branch: Hir.Branch = .{ 
                    .a_path = try self.lower_single(stmt.block),
                    .b_path = if (stmt.else_block) |else_block| try self.lower_single(else_block) else null,
                    .condition = try self.lower_single(stmt.condition)
                };
                out_node = .{ .top_level = .{ .branch = try mem.createWith(self.allocator, branch)}};
            },
            .access_operator => |expr| {
                const left = try self.lower_single(expr.left);
                const right = Hir.Ident {
                    .location = expr.right.span,
                    .value = expr.right.value,
                };
                const out: Hir.Access = .{
                    .left = left,
                    .right = right,
                };
                out_node = .{ .inline_expr = .{ .access_expr = try mem.createWith(self.allocator, out)}};
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
        .scope_id = self.current_scope,
        .adjustments = null,
        .span = ast.span,
        .ty = null
    });
    const out = Hir.Hir{ 
        .node = out_node.?,
        .id = id,
    };
    return out;

}

