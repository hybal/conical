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

pub fn init(allocator: std.mem.Allocator, context: *common.Context, file: common.FileId, source: []const u8, tree: *const Ast) @This() {
    return @This() {
        .allocator = allocator,
        .tpa = std.heap.ArenaAllocator.init(std.heap.page_allocator),
        .context = context,
        .file = file,
        .builder = .init(allocator, context, source),
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
    var oargs: std.ArrayList(hir.HirNodeId) = .empty;
    for (args) |arg| {
        try oargs.append(self.allocator, arg);
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
    const sym = try self.builder.symbol_of(right);

    const term = hir.Terminal {
        .id = sym,
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


pub fn lower(self: *@This()) !Hir.Hir {
    for (self.tree.nodes) |node| {
        _ = try self.lower_single(node);
    }
    return try self.builder.build();
}

fn lower_single(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    const kind, _ = self.tree.get(nodeid);
    const out = switch (kind) {
        .while_loop => try self.lower_while(nodeid),
        .for_loop => try self.lower_for(nodeid),
        .type_struct => try self.lower_struct(nodeid),
        .type_enum => try self.lower_enum(nodeid),
        .type_impl => try self.lower_impl(nodeid),
    };
    return out;
}

fn lower_while(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
    std.debug.assert(self.tree.get(nodeid).@"0" == .while_loop);
    const node: *ast.WhileLoop = @ptrCast(@alignCast(self.tree.get(nodeid).@"1"));
    const condition = try self.lower_single(node.condition);
    const inner_block = try self.lower_single(node.block);

    const ncond = self.unary_operator_to_overload(.bang, condition);

    const nnodes: std.ArrayList(hir.HirNodeId) = .empty;

    const brk = hir.LoopControl {
        .@"break",
    };
    const brkid = try self.builder.add_node(.loop_control, .init(0, self.file), brk);
    
    try nnodes.append(self.allocator, brkid);
    const nblock = hir.Block {
        .scope = 0,
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

    var loop_block = std.ArrayList(hir.HirNodeId).empty;

    const expr = try self.lower_single(node.expr);

    const internal_block = try self.lower_single(node.block);

    const next = try self.make_direct_access(expr, common.intrins.get_operator_overload(.next));

    const call = try self.make_fn_call(next, &.{});

    const binding = try self.make_binding(node.ident.span.a, call, null);

    try loop_block.append(self.allocator, binding);

    const null_sym = hir.Terminal {
        .literal = .{ .symbol = try self.context.intern_pool.put("next") },
    };

    const null_symid = try self.builder.add_node(.terminal, .init(0, self.file), null_sym);

    const id_term = hir.Terminal {
        .id = self.builder.symbol_of(node.ident.span.a.get_string(self.builder.source)),
    };

    const id_termid = try self.builder.add_node(.terminal, .init(0, self.file), id_term);

    const cond = try self.binary_operator_to_overload(.eq2, id_termid, null_symid);

    var cblock = std.ArrayList(hir.HirNodeId).empty;
    const brk = hir.LoopControl { .@"break" };
    const brkid = try self.builder.add_node(.loop_control, .init(0, self.file), brk);
    try cblock.append(self.allocator, brkid);

    const cblockid = try self.builder.add_node(.block, .init(0, self.file), hir.Block {
        .scope = 0,
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
        .scope = 0,
        .statements = try loop_block.toOwnedSlice(self.allocator),
    });

    const loop = try self.builder.add_node(.loop, .init(0, self.file), hir.Loop {
        .block = loop_blockid,
    });
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
}



