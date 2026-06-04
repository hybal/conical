//! Ast -> Hir Conversion
//! Removes syntax sugar and preforms symbol/name resolution.
const std = @import("std");
const ast = @import("parse").Ast;
const Ast = ast.Ast;
const common = @import("common");
const lex = @import("lex");
const hir = @import("Hir.zig");
const Hir = hir.Hir;
const diag = @import("diagnostics");
const errors = @import("errors.zig");

allocator: std.mem.Allocator,
tpa: std.mem.Allocator,
context: *common.Context,
builder: hir.HirBuilder,
file: common.FileId,
tree: *const Ast,
in_loop: bool = false,
in_func: bool = false,
in_lvalue: bool = false,

pub fn init(allocator: std.mem.Allocator, context: *common.Context, file: common.FileId, source: []const u8, tree: *const Ast) @This() {
    var tpa = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer tpa.deinit();
    return @This() {
        .allocator = allocator,
        .tpa = tpa.allocator(),
        .context = context,
        .file = file,
        .builder = .init(allocator, source, context),
        .tree = tree,
    };
}

pub fn deinit(self: *@This()) void {
    self.builder.deinit();
}

fn binary_operator_to_overload(self: *@This(), op: lex.Tag, left: hir.HirNodeId, right: hir.HirNodeId) !hir.HirNodeId {
    const op_type: common.intrins.OverloadOp = switch(op) {
        .plus => .add,
        .minus => .sub,
        .star => .mul,
        .slash => .div,
        .lt2 => .lshift,
        .gt2 => .rshift,
        .pipe => .bitor,
        .amp => .bitand,
        .caret => .bitxor,
        .eq2 => .eq,
        .bangeq => .neq,
        .lt => .lt,
        .gt => .gt,
        .lteq => .lteq,
        .gteq => .gteq,
        .amp2 => .logand,
        .pipe2 => .logor,
        else => unreachable,
    };

    const funcidstr = common.intrins.get_operator_overload(op_type);

    const symbol = self.builder.symbol_of(funcidstr).?;

    const term = hir.Terminal {
        .id = symbol,
    };
    const termid = try self.builder.add_node(.terminal, .init(0, self.file), term);

    const access = hir.Access {
        .left = left,
        .right = termid,
    };

    const accessid = try self.builder.add_node(.access, .init(0, self.file), access);

    const call = try self.make_fn_call(accessid, &.{ right });
    return call;
}

fn unary_operator_to_overload(self: *@This(), op: lex.Tag, expr: hir.HirNodeId) !hir.HirNodeId {
    const op_type: common.intrins.OverloadOp = switch(op) {
        .bang => .lognot,
        else => unreachable,
    };

    const funcidstr = common.intrins.get_operator_overload(op_type);

    const symbol = self.builder.symbol_of(funcidstr).?;

    const term = hir.Terminal {
        .id = symbol,
    };

    const termid = try self.builder.add_node(.terminal, .init(0, self.file), term);

    const access = hir.Access {
        .left = expr,
        .right = termid,
    };

    const accessid = try self.builder.add_node(.access, .init(0, self.file), access);

    const call = try self.make_fn_call(accessid, &.{});
    return call;
}

fn make_fn_call(self: *@This(), left: hir.HirNodeId, args: []const hir.HirNodeId) !hir.HirNodeId {
    var oargs: std.ArrayList(hir.FnArg) = .empty;
    for (args) |arg| {
        try oargs.append(self.allocator, hir.FnArg { .expr = arg, .is_generic = false, .param_name = null });
    }

    const call = hir.FnCall {
        .left = left,
        .args = try oargs.toOwnedSlice(self.allocator),
    };
    const callid = try self.builder.add_node(.fn_call, .init(0, self.file), call);
    return callid;
}


fn make_binding(self: *@This(), id: common.Span, expr: hir.HirNodeId, mod: ?hir.BindingModifier) !hir.HirNodeId {
    const sym = try self.builder.add_symbol(id);
    const intern = self.builder.get_symbol(sym).id;

    const binding = hir.Binding {
        .id = intern,
        .initialization = expr,
        .modifier = mod,
    };

    const binding_id = try self.builder.add_node(.binding, .init(0, self.file), binding);
    return binding_id;
}

fn make_direct_access(self: *@This(), left: hir.HirNodeId, right: []const u8) !hir.HirNodeId {
    const sym = self.builder.symbol_of(right);
    if (sym == null) {
        unreachable;
    }

    const term = hir.Terminal {
        .id = sym.?,
    };

    const rightid = try self.builder.add_node(.terminal, .init(0, self.file), term);

    const access = hir.Access {
        .left = left,
        .right = rightid,
    };

    const accessid = try self.builder.add_node(.access, .init(0, self.file), access);
    return accessid;
}

fn make_multitype(self: *@This(), op: hir.TypeBinaryExprType, vals: []const hir.HirNodeId) !hir.HirNodeId {
    var expr: hir.HirNodeId = vals[0];
    if (vals.len == 0) unreachable;
    if (vals.len % 2 == 0) unreachable;
    for (1..vals.len) |i| {
        expr = try self.builder.add_node(.type_binary_expr, .init(0, self.file), hir.TypeBinaryExpr {
            .left = expr,
            .right = vals[i],
            .ty = op,
        });
    }
    return expr;
}

fn make_label(self: *@This(), id: common.Span, expr: hir.HirNodeId) !hir.HirNodeId {
    const intern = try self.context.intern_pool.put(id.get_string(self.builder.source));

    const label = hir.TypeLabel {
        .id = intern,
        .expr = expr,
    };
    const labelid = try self.builder.add_node(.type_label, .init(0, self.file), label);
    return labelid;
}

fn make_block(self: *@This(), scope: hir.ScopeId, mod: ?hir.EvalModifier, exprs: []const hir.HirNodeId) !hir.HirNodeId {
    const expr_arr = try self.allocator.dupe(hir.HirNodeId, exprs);
    const block = hir.Block {
        .scope = scope,
        .statements = expr_arr,
        .mod = mod,
    };

    const blockid = try self.builder.add_node(.block, .init(0, self.file), block);
    return blockid;
}

fn ident_to_intern(self: *@This(), id: ast.Ident) !common.intern.InternId {
    if (id.span.is_b()) {
        return error.InvalidIdentifier;
    }
    return try self.context.intern_pool.put(id.span.a.get_string(self.builder.source));
}


pub fn lower(self: *@This()) !Hir {
    for (self.tree.nodes, 0..) |_, i| {
        _ = try self.lower_single(i);
    }
    return try self.builder.build();
}

fn lower_single(self: *@This(), nodeid: ast.AstNodeId) anyerror!hir.HirNodeId {
    const kind, const node = self.tree.get(nodeid);
    const out = switch (kind) {
        .while_loop => try self.lower_while(nodeid),
        .for_loop => try self.lower_for(nodeid),
        .type_struct => try self.lower_struct(nodeid),
        .type_enum => try self.lower_enum(nodeid),
        .type_impl => try self.lower_impl(nodeid),
        .binary_expr => try self.lower_binary_expr(nodeid),
        .unary_expr => try self.lower_unary_expr(nodeid),
        .item => try self.lower_item(nodeid),
        .access_operator => try self.lower_access(nodeid),
        .assignment => try self.lower_assignment(nodeid),
        .block => try self.lower_block(nodeid, null),
        .cast => try self.lower_cast(nodeid),
        .fn_call => try self.lower_fn_call(nodeid),
        .if_stmt => try self.lower_if_stmt(nodeid),
        .loop => try self.lower_loop(nodeid),
        .loop_control => try self.lower_loop_control(nodeid),
        .mod_block => mb: {
            const n: *ast.ModBlock = @ptrCast(@alignCast(node));
            break :mb try self.lower_block(nodeid, n.mod);
        },
        .import => try self.lower_import(nodeid),
        .index => try self.lower_index(nodeid),
        .initializer => try self.lower_initializer(nodeid),
        .terminal => try self.lower_terminal(nodeid),
        .terminated => try self.lower_terminated(nodeid),
        .lambda => try self.lower_lambda(nodeid),
        .path => try self.lower_path(nodeid),
        .return_stmt => try self.lower_return(nodeid),
        .slice => try self.lower_slice(nodeid),
        .type_binary_expr => try self.lower_type_binary_expr(nodeid),
        .type_set => try self.lower_type_set(nodeid),
        .type_range => try self.lower_type_range(nodeid),
        .type_decl => try self.lower_type_decl(nodeid),
        .type_literal => try self.lower_type_literal(nodeid),
        .type_label => try self.lower_type_label(nodeid),
        .type_metadata => try self.lower_type_metadat(nodeid),
        .type_modifier => try self.lower_type_modifier(nodeid),
        else => |e| {
            std.debug.print("UNIMPLEMENTED: {any}\n", .{e});
            unreachable;
        },
    };
    return out;
}
fn lower_type_set(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .type_set);
    const node: *ast.TypeSet = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    var nodes: std.ArrayList(hir.HirNodeId) = .empty;
    for (node.values) |v| {
        try nodes.append(self.allocator, try self.lower_single(v));
    }
    const set = hir.TypeSet {
        .values = try nodes.toOwnedSlice(self.allocator),
    };
    const term = hir.TypeTerminal {
        .set = set,
    };
    const out = try self.builder.add_node(.type_terminal, self.tree.get_span(nodeid), term);
    return out;
}
 
fn lower_type_binary_expr(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .type_binary_expr);
    const node: *ast.TypeBinaryExpr = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    const left = try self.lower_single(node.left);
    const right = try self.lower_single(node.right);
    const op: hir.TypeBinaryExprType = switch (node.op) {
        .Difference => .difference,
        .Equality => .equality,
        .Intersection => .intersection,
        .Membership => .membership,
        .Product => .product,
        .StrictSubset => .strict_subset,
        .StrictSuperSet => .strict_superset,
        .Subset => .subset,
        .SuperSet => .superset,
        .Union => .@"union",
    };

    const expr = hir.TypeBinaryExpr {
        .left = left,
        .right = right,
        .ty = op
    };
    const out = try self.builder.add_node(.type_binary_expr, self.tree.get_span(nodeid), expr);
    return out;
}
 fn lower_slice(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .slice);
    const node: *ast.SliceOp = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    _ = node;
    unreachable;

} 

fn lower_return(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .return_stmt);
    const node: *ast.ReturnStmt = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    if (!self.in_func) {
        return error.ReturnOutsideOfFunc;
    }
    const ret = hir.Return {
        .expr = try self.lower_single(node.expr),
    };
    const out = try self.builder.add_node(.return_stmt, self.tree.get_span(nodeid), ret);
    return out;
}
 

fn lower_path(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .path);
    const node: *ast.Path = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    _ = node;
    unreachable;
}
 

fn lower_lambda(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .lambda);
    const node: *ast.Lambda = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    var generics: []hir.Generic = try self.allocator.alloc(hir.Generic, node.generics.len);
    const saved_scope = self.builder.scope;
    _ = try self.builder.add_scope(true);
    for (node.generics, 0..) |generic, i| {
        const ty = if (generic.expr) |e| try self.lower_single(e) else null;
        const id = try self.ident_to_intern(generic.ident);
        _ = try self.builder.add_symbol(generic.ident.span.a);
        generics[i] = hir.Generic {
            .id = id,
            .ty = ty,
        };
    }
    var params: []hir.LambdaParameter = try self.allocator.alloc(hir.LambdaParameter, node.params.len);
    for (node.params, 0..) |param, i| {
        const mod: ?hir.BindingModifier = if (param.mod) |m| switch (m.kind) {
            .move => .move,
            .mut => .mut,
            .alias => .alias,
        } else null;
        _ = try self.builder.add_symbol(param.ident.span.a);
        params[i] = hir.LambdaParameter {
            .id = try self.ident_to_intern(param.ident),
            .ty = if (param.ty) |pt| try self.lower_single(pt) else null,
            .modifier = mod,
        };
    }
    const ret_ty: ?hir.HirNodeId = if (node.ret_ty) |r| try self.lower_single(r) else null;
    const body = try self.lower_single(node.expr);
    self.builder.into_scope(saved_scope);
    const lambda = hir.Lambda {
        .block = body,
        .generics = generics,
        .params = params,
        .return_ty = ret_ty,
    };
    const out = try self.builder.add_node(.lambda, self.tree.get_span(nodeid), lambda);
    return out;
}

fn lower_terminated(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .terminated);
    const node: *ast.Terminated = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    return try self.lower_single(node.expr);
}
 
fn lower_terminal(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .terminal);
    const node: *ast.Terminal = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    const term = node.termtype;
    switch (term) {
        .symbol => |sym| {
            const internid = try self.context.intern_pool.put(sym.span.a.get_string(self.builder.source));
            const termnode = hir.Terminal {
                .literal = .{
                    .symbol = internid,
                },
            };
            const out = try self.builder.add_node(.terminal, self.tree.get_span(nodeid), termnode);
            return out;
        },
        .value => |tok| {
            _ = tok;
            return error.Unimplemented;
        },
    }
}
 

fn lower_initializer(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .initializer);
    const node: *ast.Initializer = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    var tyexpr: ?hir.HirNodeId = null;
    if (node.ty) |t| {
        tyexpr = try self.lower_single(t);
    }
    var compounds: []hir.CompoundLiteralValue = try self.allocator.alloc(hir.CompoundLiteralValue, node.fields.len);
    for (node.fields, 0..) |field, i| {
        const label: ?common.intern.InternId = if (field.id) |id| try self.context.intern_pool.put(id.span.a.get_string(self.builder.source)) else null;
        compounds[i] = hir.CompoundLiteralValue {
            .label = label,
            .value = try self.lower_single(field.value),
        };
    }
    const compound = hir.CompoundLiteral {
        .values = compounds,
        .ty = tyexpr,
    };
    const term = hir.Terminal {
        .compound = compound,
    };
    const out = try self.builder.add_node(.terminal, self.tree.get_span(nodeid), term);
    return out;
}
 

fn lower_index(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .index);
    const node: *ast.IndexOp = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    const op: common.intrins.OverloadOp = if (self.in_lvalue) .index_set else .index;
    const expr = try self.lower_single(node.expr);
    const index_expr = try self.lower_single(node.index);
    const func = common.intrins.get_operator_overload(op);
    const access = try self.make_direct_access(expr, func);
    const callnode = try self.make_fn_call(access, &.{index_expr});

    const deref = hir.UnaryExpr {
        .expr = callnode,
        .ty = .dereference,
    };
    const out = try self.builder.add_node(.unary_expr, self.tree.get_span(nodeid), deref);
    return out;
}
 

fn lower_import(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .import);
    const node: *ast.Import = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    _ = node;
    return error.Unimplemented;
}
 
fn lower_loop_control(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .loop_control);
    const node: *ast.LoopControl = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    if (!self.in_loop) {
        return error.LoopControlOutsideOfLoop;
    }
    const control: hir.LoopControl = switch (node.control) {
        .@"break" => .@"break",
        .@"continue" => .@"continue",
    };
    const out = try self.builder.add_node(.loop_control, self.tree.get_span(nodeid), control);
    return out;
}
 
fn lower_loop(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .loop);
    const node: *ast.Loop = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    const saved_in_loop = self.in_loop;
    self.in_loop = true;
    const block = try self.lower_single(node.block);
    self.in_loop = saved_in_loop;
    const loop = hir.Loop {
        .block = block,
    };
    const out = try self.builder.add_node(.loop, self.tree.get_span(nodeid), loop);
    return out;
}


fn lower_if_stmt(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .if_stmt);
    const node: *ast.IfStmt = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    const condition = try self.lower_single(node.condition);
    const prev_scope = self.builder.scope;
    var refinements: ?[]hir.RefinementBinding = null;
    if (node.refinements) |refids| {
        refinements = try self.allocator.alloc(hir.RefinementBinding, refids.len);
        _ = try self.builder.add_scope(true);
        for (refids, 0..) |refid, i| {
            _ = try self.builder.add_symbol(refid.a.span.a);
            var sym: hir.SymbolId = undefined;
            if (refid.b) |b| {
                if (self.builder.symbol_of(b.span.a.get_string(self.builder.source))) |s| {
                    sym = s;
                } else {
                    return error.UndefinedSymbol;
                }
            } else {
                if (self.builder.symbol_of(refid.a.span.a.get_string(self.builder.source))) |s| {
                    sym = s;
                } else {
                    return error.UndefinedSymbol;
                }
            }
            refinements.?[i] = hir.RefinementBinding {
                .left = try self.context.intern_pool.put(refid.a.span.a.get_string(self.builder.source)),
                .right = sym,
            };

        }
    }
    const then = try self.lower_single(node.block);
    const @"else" = if (node.else_block) |e| try self.lower_single(e) else null;
    self.builder.into_scope(prev_scope);
    const conditional = hir.Conditional {
        .condition = condition,
        .refinements = refinements,
        .@"else" = @"else",
        .then = then,
    };

    const out = try self.builder.add_node(.conditional, self.tree.get_span(nodeid), conditional);
    return out;

}

fn lower_fn_call(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .fn_call);
    const node: *ast.FnCall = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    const left = try self.lower_single(node.left);

    var args = try self.allocator.alloc(hir.FnArg, node.params.len);

    for (node.params, 0..) |arg, i| {
        const id: ?common.intern.InternId = if (arg.id) |id| try self.context.intern_pool.put(id.span.a.get_string(self.builder.source)) else null;
        args[i] = hir.FnArg {
            .expr = try self.lower_single(arg.val),
            .param_name = id,
            .is_generic = arg.is_generic,
        };
    }
    const call = hir.FnCall {
        .args = args,
        .left = left,
    };
    const out = try self.builder.add_node(.fn_call, self.tree.get_span(nodeid), call);
    return out;
}

fn lower_cast(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    _ = self;
    _ = nodeid;
    unreachable;
}


fn lower_block(self: *@This(), nodeid: ast.AstNodeId, mod: ?ast.EvalModifier) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .block);
    const node: *ast.Block = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    var block_nodes = try self.allocator.alloc(hir.HirNodeId, node.exprs.len);
    const prev_scope = self.builder.scope;
    const scope = try self.builder.add_scope(true);
    for (node.exprs, 0..) |e, i| {
        block_nodes[i] = try self.lower_single(e);
    }
    self.builder.into_scope(prev_scope);
    const modifier: ?hir.EvalModifier = if (mod) |m| switch (m) {
        .@"inline" => .@"inline",
        .@"comptime" => .@"comptime",
        .pure => .pure,
    } else null;
    const block = hir.Block {
        .scope = scope,
        .mod = modifier,
        .statements = block_nodes,
    };
    const out = try self.builder.add_node(.block, self.tree.get_span(nodeid), block);
    return out;
}

fn lower_assignment(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .assignment);
    const node: *ast.Assignment = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    const expr = try self.lower_single(node.expr);
    const saved_in_lvalue = self.in_lvalue;
    self.in_lvalue = true;
    const lvalue = try self.lower_single(node.lvalue);
    self.in_lvalue = saved_in_lvalue;
    const assignment = hir.Assignment {
        .left = lvalue,
        .right = expr,
    };
    const out = try self.builder.add_node(.assignment, self.tree.get_span(nodeid), assignment); 
    return out;
}

fn lower_access(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .access_operator);
    const node: *ast.AccessOperator = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    const left = try self.lower_single(node.left);
    const right = try self.context.intern_pool.put(node.right.span.get_string(self.builder.source));
    const access: hir.Access = .{
        .left = left,
        .right = right,
    };
    const out = try self.builder.add_node(.access, self.tree.get_span(nodeid), access);
    return out;
}


fn lower_item(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .item);
    const node: *ast.Item = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    const item_node = switch (node.item_kind) {
        .function => try self.lower_fn_decl(node.item, node.function_mods),
        .binding => try self.lower_binding(node.item),
    };
    const linkage: ?hir.Linkage = if (node.linkage) |l| switch (l.kind) {
        .@"export" => .@"export",
        .@"extern" => .@"extern",
    } else null;
    const visibility: ?hir.Visibility = if (node.visibility) |v| switch (v.kind) {
        .public => .public,
    } else null;
    const item = hir.Item {
        .linkage = linkage,
        .node = item_node,
        .visibility = visibility,
        .kind = switch (node.item_kind) {
            .binding => .binding,
            .function => .func,
        },
    };
    const out = try self.builder.add_node(.item, self.tree.get_span(nodeid), item);
    return out;
}

fn lower_binding(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .var_decl);
    const node: *ast.VarDecl = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    const expr = try self.lower_single(node.initialize);
    _ = try self.builder.add_symbol(node.id.id.span.a);
    const mod: ?hir.BindingModifier = if (node.id.modifier) |m| switch (m.kind) {
        .alias => .alias,
        .mut => .mut,
        .move => .move,
    } else null;
    const binding = hir.Binding {
        .id = try self.context.intern_pool.put(node.id.id.span.a.get_string(self.builder.source)),
        .initialization = expr,
        .modifier = mod,
    };
    const out = try self.builder.add_node(.binding, self.tree.get_span(nodeid), binding);
    return out;
    
}
fn lower_fn_decl(self: *@This(), nodeid: ast.AstNodeId, mods: ?[]ast.FnMod) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .fn_decl);
    const node: *ast.FnDecl = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    const id = try self.context.intern_pool.put(node.ident.span.a.get_string(self.builder.source));
    const prev_scope = self.builder.scope;
    _ = try self.builder.add_scope(true);
    var generics_buf = try self.allocator.alloc(hir.Generic, node.generics.len);
    for (node.generics, 0..) |gen, i| {
        generics_buf[i] = hir.Generic {
            .id = try self.context.intern_pool.put(gen.ident.span.a.get_string(self.builder.source)),
            .ty = if (gen.expr) |e| try self.lower_single(e) else null,
        };
        _ = try self.builder.add_symbol(gen.ident.span.a);
    }
    var param_buf = try self.allocator.alloc(hir.FunctionParameter, node.params.len);
    for (node.params, 0..) |p, i| {
        const param_mod: ?hir.BindingModifier = if (p.modifier) |m| switch (m.kind) {
            .alias => .alias,
            .mut => .mut,
            .move => .move,
        } else null;
        param_buf[i] = hir.FunctionParameter {
            .id = try self.context.intern_pool.put(p.id.span.a.get_string(self.builder.source)),
            .ty = try self.lower_single(node.param_types[i]),
            .modifier = param_mod,
        };
        _ = try self.builder.add_symbol(p.id.span.a);
    }
    const ret_ty = if (node.return_ty) |r| try self.lower_single(r) else null;
    const block = try self.lower_single(node.body);
    self.builder.into_scope(prev_scope);
    var decl_mods: ?[]hir.FnModifier = null;
    if (mods) |ms| {
        decl_mods = try self.allocator.alloc(hir.FnModifier, ms.len);
        for (ms, 0..) |m, i| {
            decl_mods.?[i] = switch (m.kind) {
                .@"comptime" => .@"comptime",
                .@"inline" => .@"inline",
                .pure => .pure,
            };
        }
    }
    const out_node = hir.FnDecl {
        .generics = generics_buf,
        .id = id,
        .params = param_buf,
        .ret_ty = ret_ty,
        .modifiers = decl_mods,
        .body = block,
    };
    const out = try self.builder.add_node(.fn_decl, self.tree.get_span(nodeid), out_node);
    return out;
}

fn lower_binary_expr(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .binary_expr);
    const node: *ast.BinaryExpr = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    const left = try self.lower_single(node.left);
    const right = try self.lower_single(node.right);
    const op = try self.binary_operator_to_overload(node.op.tag, left ,right);
    return op;
}

fn lower_unary_expr(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .unary_expr);
    const node: *ast.UnaryExpr = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    const expr = try self.lower_single(node.expr);
    const op = try self.unary_operator_to_overload(node.op.tag, expr);
    return op;
}


fn lower_while(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .while_loop);
    const node: *ast.WhileLoop = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    const condition = try self.lower_single(node.condition);
    const saved_in_loop = self.in_loop;
    self.in_loop = true;
    const inner_block = try self.lower_single(node.block);
    self.in_loop = saved_in_loop;

    const ncond = try self.unary_operator_to_overload(.bang, condition);

    var nnodes: std.ArrayList(hir.HirNodeId) = .empty;

    const brk = hir.LoopControl.@"break";
    const brkid = try self.builder.add_node(.loop_control, .init(0, self.file), brk);
    
    try nnodes.append(self.allocator, brkid);
    const nblock = hir.Block {
        .scope = 0,
        .mod = null,
        .statements = try nnodes.toOwnedSlice(self.allocator),
    };
    const nblockid = try self.builder.add_node(.block, .init(0, self.file), nblock);

    const conditional = hir.Conditional {
        .condition = ncond,
        .then = nblockid,
        .@"else" = null,
        .refinements = null
    };

    const conditionalid = try self.builder.add_node(.conditional, .init(0, self.file), conditional);
    var nodes: std.ArrayList(hir.HirNodeId) = .empty;

    try nodes.append(self.allocator, inner_block);
    try nodes.append(self.allocator, conditionalid);

    const block = hir.Block {
        .mod = null,
        .scope = 0,
        .statements = try nodes.toOwnedSlice(self.allocator),
    };
    const blockid = try self.builder.add_node(.block, .init(0, self.file), block);

    const loop = hir.Loop {
        .block = blockid,
    };

    const loopid = try self.builder.add_node(.loop, .init(0, self.file), loop);
    return loopid;
}

fn lower_for(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .for_loop);
    const node: *ast.ForLoop = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    if (node.ident.span.is_b()) {
        return error.Error;
    }
    const saved_scope = self.builder.scope;
    _ = try self.builder.add_scope(true);
    var loop_block = std.ArrayList(hir.HirNodeId).empty;

    const expr = try self.lower_single(node.expr);
    
    const saved_in_loop = self.in_loop;
    self.in_loop = true;
    const internal_block = try self.lower_single(node.block);
    self.in_loop = saved_in_loop;

    const next = try self.make_direct_access(expr, common.intrins.get_operator_overload(.next));

    const call = try self.make_fn_call(next, &.{});

    const binding = try self.make_binding(node.ident.span.a, call, null);

    try loop_block.append(self.allocator, binding);

    const null_sym = hir.Terminal {
        .literal = .{ .symbol = try self.context.intern_pool.put("next") },
    };

    const null_symid = try self.builder.add_node(.terminal, .init(0, self.file), null_sym);

    const id_term = hir.Terminal {
        .id = self.builder.symbol_of(node.ident.span.a.get_string(self.builder.source)).?,
    };

    const id_termid = try self.builder.add_node(.terminal, .init(0, self.file), id_term);

    const cond = try self.binary_operator_to_overload(.eq2, id_termid, null_symid);

    var cblock = std.ArrayList(hir.HirNodeId).empty;
    const brk = hir.LoopControl.@"break";
    const brkid = try self.builder.add_node(.loop_control, .init(0, self.file), brk);
    try cblock.append(self.allocator, brkid);

    const cblockid = try self.builder.add_node(.block, .init(0, self.file), hir.Block {
        .scope = 0,
        .mod = null,
        .statements = try cblock.toOwnedSlice(self.allocator),
    });

    const conditional = hir.Conditional {
        .condition = cond,
        .then = cblockid,
        .@"else" = null,
        .refinements = null,
    };

    const conditionalid = try self.builder.add_node(.conditional, .init(0, self.file), conditional);

    try loop_block.append(self.allocator, conditionalid);

    try loop_block.append(self.allocator, internal_block);

    const loop_blockid = try self.builder.add_node(.block, .init(0, self.file), hir.Block {
        .mod = null,
        .scope = 0,
        .statements = try loop_block.toOwnedSlice(self.allocator),
    });

    const loop = try self.builder.add_node(.loop, .init(0, self.file), hir.Loop {
        .block = loop_blockid,
    });
    self.builder.into_scope(saved_scope);
    return loop;
}


fn lower_struct(self: *@This(), nodeid: hir.HirNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .type_struct);
    const node: *ast.TypeStruct = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    
    var exprs: std.ArrayList(hir.HirNodeId) = try .initCapacity(self.tpa, node.field_exprs.len);
    defer exprs.deinit(self.tpa);
    for (node.field_exprs, node.field_labels) |ex, lbl| {
        const e = try self.make_label(lbl.span.a, try self.lower_single(ex));
        try exprs.append(self.tpa, e);
    }
    const prod = try self.make_multitype(.product, exprs.items);
    return prod;
}

fn lower_enum(self: *@This(), nodeid: hir.HirNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .type_enum);
    const node: *ast.TypeEnum = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));

    var exprs: std.ArrayList(hir.HirNodeId) = try .initCapacity(self.tpa, node.variants.len);
    defer exprs.deinit(self.tpa);
    for (node.variants) |v| {
        const e = try self.lower_single(v);
        try exprs.append(self.tpa, e);
    }
    const un = try self.make_multitype(.@"union", exprs.items);
    return un;
}

fn lower_impl(self: *@This(), nodeid: hir.HirNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .type_impl);
    const node: *ast.TypeImpl = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    
    for (node.declarations) |decl| {
        const item: *ast.Item = @ptrCast(@alignCast(self.tree.get(decl).@"1"));
        if (item.linkage != null) {
            //ERROR: Sets currently can't have non-internal linkage
            return error.HirError;
        }

        if (item.visibility != null) {
            //ERROR: Sets currently default to public and can't be changed.
            return error.HirError;
        }
    }
    return error.Unimplemented;
}





