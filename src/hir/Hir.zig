const std = @import("std");
const ast = @import("parse");
const Ast = ast.Ast;
const common = @import("common");
const diag = @import("diagnostics");

pub const Poison = struct {
    error_id: diag.ErrorId,
};

pub const Terminal = union(enum) {

};

pub const HirNodeId = usize;
pub const HirId = struct {
    owner: HirNodeId,
    local: HirNodeId,
};

pub const HirKind = enum {
    poison,
    terminal,
    unary_expr,
    access,
    assignment,
    conditional,
    loop,
    loop_control,
    match,
    block,
    binding,
    type_expr,
    type_binary_expr,
    type_literal,
    type_label,
    type_set,
    type_range,
    return_stmt,
    import,
    cast,
    index,
    initializer,
    lambda,
};

pub const SpanId = ast.SpanId;

pub const ScopeId = usize;

pub const Symbol = struct {
    span: SpanId,
    id: common.intern.InternId,


};

pub const SymbolTable = [][]Symbol; //[ScopeId][SymbolId]

pub const HirNode = struct {
    kind: HirKind,
    span: SpanId,
    index: usize,
};

pub const Hir = struct {
    symbol_map: std.StringHashMap(Symbol),
    symbol_table: SymbolTable,
    nodes: []HirNode,
};

