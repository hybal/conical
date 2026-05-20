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
context: *common.Context,
builder: hir.HirBuilder,
file: common.FileId,
tree: *const Ast,

pub fn init(allocator: std.mem.Allocator, context: *common.Context, file: common.FileId, source: []const u8, tree: *const Ast) @This() {
    return @This() {
        .allocator = allocator,
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
    };
    return out;
}

fn lower_while(self: *@This(), nodeid: ast.AstNodeId) !hir.HirNodeId {
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


