const std = @import("std");
const ast = @import("parse");
const Ast = ast.Ast;
const common = @import("common");
const diag = @import("diagnostics");

const Int = void;
const Float = void;

pub const Poison = struct {
    error_id: diag.ErrorId,
};

pub const Terminal = union(enum) {
    literal: Literal,
    id: SymbolId,
    compound: CompoundLiteral,
};

pub const Literal = union(enum) {
    integer: Int,
    float: Float,
    string: InternId,
    @"bool": bool,
    symbol: InternId
};

pub const CompoundLiteral = struct {
    ty: ?HirNodeId,
    values: []const CompoundLiteralValue,
};

pub const CompoundLiteralValue = struct {
    label: ?InternId,
    value: HirNodeId,
};

pub const Terminated = struct {
    node: HirNodeId,
};

pub const UnaryExpr = struct {
    ty: UnaryExprType,
    expr: HirNodeId,
};

pub const UnaryExprType = enum {
    reference,
    dereference,
};

pub const Access = struct {
    left: HirNodeId,
    right: HirNodeId,
};

pub const Assignment = struct {
    left: HirNodeId,
    right: HirNodeId,
};

pub const Conditional = struct {
    condition: HirNodeId,
    then: HirNodeId,
    @"else": ?HirNodeId,
    refinements: ?[]RefinementBinding,
};

pub const RefinementBinding = struct {
    left: InternId,
    right: SymbolId,
};

pub const Loop = struct {
    block: HirNodeId,
};

pub const LoopControl = enum {
    @"break",
    @"continue",
};

pub const Match = struct {
    expr: HirNodeId,
    arms: []MatchArm,
};

pub const MatchArm = struct {
    patterns: []HirNodeId,
    captures: []InternId,
    block: HirNodeId,
};

pub const Pattern = union(enum) {
    literal: HirNodeId,
    range: HirNodeId,
    compound: HirNodeId,
    underscore,
};

pub const EvalModifier = enum {
    @"inline",
    @"comptime",
    pure,
};

pub const Block = struct {
    statements: []HirNodeId,
    mod: ?EvalModifier,
    scope: ScopeId,
};

pub const Binding = struct {
    id: InternId,
    modifier: ?BindingModifier,
    initialization: HirNodeId,
};

pub const BindingModifier = enum {
    alias,
    move,
    mut,
};

pub const TypeBinaryExprType = enum {
    @"union",
    product,
    intersection,
    difference,
    membership,
    subset,
    superset,
    strict_subset,
    strict_superset,
};
pub const TypeBinaryExpr = struct {
    ty: TypeBinaryExprType,
    left: HirNodeId,
    right: HirNodeId,
};

pub const TypeSet = struct {
    values: []HirNodeId,
};
pub const TypeTerminal = union(enum) {
    literal: TypeLiteral,
    ident: SymbolId,
    range: TypeRange,
    label: TypeLabel,
    set: TypeSet,
};
pub const TypeLiteral = union(enum) {
    literal: Literal,
    Self,
};

pub const TypeRange = struct {
    left: Literal,
    right: Literal,
    left_inclusive: bool,
    right_inclusive: bool
};

pub const TypeLabel = struct {
    id: InternId, 
    expr: HirNodeId,
};

pub const Return = struct {
    expr: HirNodeId,
};

//NOTE: May be removed
pub const Cast = struct {
    left: HirNodeId,
    right: HirNodeId,
};

pub const FunctionParameter = struct {
    id: InternId, 
    ty: HirNodeId,
    modifier: ?BindingModifier,
};

pub const LambdaParameter = struct {
    id: InternId,
    ty: ?HirNodeId,
    modifier: ?BindingModifier,
};

pub const Generic = struct {
    id: InternId, 
    ty: ?HirNodeId,
};

pub const Lambda = struct {
    params: []LambdaParameter,
    generics: []Generic,
    return_ty: ?HirNodeId,
    block: HirNodeId,
};

pub const FnArg = struct {
    expr: HirNodeId,
    is_generic: bool,
    param_name: ?InternId,
};
pub const FnCall = struct {
    left: HirNodeId,
    args: []FnArg,
};

pub const FnDecl = struct {
    id: InternId, 
    params: []FunctionParameter,
    generics: []Generic,
    ret_ty: ?HirNodeId,
    modifiers: ?[]FnModifier,
    body: HirNodeId,
};

pub const FnModifier = enum {
    pure,
    @"inline",
    @"comptime",
};

pub const ItemKind = enum {
    binding,
    func
};

pub const Linkage = enum {
    @"export",
    @"extern",
};

pub const Visibility = enum {
    public,
};

pub const Item = struct {
    kind: ItemKind,
    node: HirNodeId,
    linkage: ?Linkage,
    visibility: ?Visibility,
};

pub const HirNodeId = usize;

pub const HirKind = enum {
    poison,
    terminal,
    terminated,
    unary_expr,
    access,
    assignment,
    conditional,
    loop,
    loop_control,
    match,
    pattern,
    block,
    binding,
    type_binary_expr,
    type_terminal,
    type_label,
    return_stmt,
    cast,
    lambda,
    fn_call,
    fn_decl,
    item,
};

const HirNodeType = struct {
    poison: Poison,
    terminal: Terminal,
    terminated: Terminated,
    unary_expr: UnaryExpr,
    access: Access,
    assignment: Assignment,
    conditional: Conditional,
    loop: Loop,
    loop_control: LoopControl,
    match: Match,
    pattern: Pattern,
    block: Block,
    binding: Binding,
    type_binary_expr: TypeBinaryExpr,
    type_terminal: TypeTerminal,
    type_label: TypeLabel,
    return_stmt: Return,
    cast: Cast,
    lambda: Lambda,
    fn_call: FnCall,
    fn_decl: FnDecl,
    item: Item,
};

fn HirNodeTypeFinal() type {
    const info = @typeInfo(HirNodeType).@"struct";
    var new_fields_types: [info.fields.len]type = undefined;
    var new_fields_names: [info.fields.len][]const u8 = undefined;
    inline for (info.fields, 0..) |field, i| {
        new_fields_types[i] = []field.type;
        new_fields_names[i] = field.name;
    }
    return @Struct(
        .auto,
        null,
        &new_fields_names,
        &new_fields_types,
        &@splat(.{}),
    );
}

fn HirNodeTypeBuilder() type {
    const info = @typeInfo(HirNodeType).@"struct";
    var new_fields_types: [info.fields.len]type = undefined;
    var new_fields_names: [info.fields.len][]const u8 = undefined;
    inline for (info.fields, 0..) |field, i| {
        new_fields_types[i] = std.ArrayList(field.type);
        new_fields_names[i] = field.name;
    }
    return @Struct(
        .auto,
        null,
        &new_fields_names,
        &new_fields_types,
        &@splat(.{}),
    );

}

pub const SpanId = ast.Ast.SpanId;

pub const ScopeId = usize;

pub const SymbolId = usize;

pub const InternId = common.intern.InternId;

pub const Symbol = struct {
    span: common.Span,
    id: InternId,
};

/// A symbol table
/// Note this is not the global symbol table for declarations
///  it is purely for use when doing symbol resolution during Hir -> Tir conversion
pub const SymbolTable = [][]Symbol; //[ScopeId][SymbolId]

pub const HirNode = struct {
    kind: HirKind,
    span: SpanId,
    index: usize,
};

pub const Import = struct {
    path: SymbolId,
};

pub const Hir = struct {
    symbol_map: std.StringHashMap(SymbolId),
    symbol_table: SymbolTable,
    imports: []Import,
    spans: []common.Span,
    nodes: []HirNode,
    internal_nodes: HirNodeTypeFinal(),

    pub fn get(self: *const @This(), id: HirNodeId) struct {HirKind, *anyopaque} {
        const node = self.get_node(id);
        const node_index = node.index;
        const node_kind = node.kind;
        inline for (comptime std.meta.tags(HirKind)) |tag| {
            if (node_kind == tag) {
                return .{
                    tag,
                    @constCast(&@field(self.internal_nodes, @tagName(tag))[node_index]),
                };
            }
        }
        unreachable;
    }

    pub fn get_node(self: *const @This(), id: HirNodeId) HirNode {
        return self.nodes[id];
    }

    pub fn get_span(self: *const @This(), id: HirNodeId) common.Span {
        return self.spans[self.get_node(id).span];
    }

    pub fn is_poison(self: *const @This(), id: HirNodeId) bool {
        return self.get_node(id).kind == .poison;
    }

    pub fn deinit(self: *const @This(), allocator: std.mem.Allocator) void {
        inline for (comptime std.meta.fieldNames(HirNodeTypeFinal())) |fl| {
            allocator.free(@field(self.internal_nodes, fl));
        }
    }
};

pub const HirBuilder = struct {
    symbol_map: std.StringHashMap(SymbolId),
    symbol_table: std.ArrayList(std.ArrayList(Symbol)),
    allocator: std.mem.Allocator,
    spans: std.ArrayList(common.Span),
    nodes: std.ArrayList(HirNode),
    internal_nodes: HirNodeTypeBuilder(),
    source: []const u8,
    context: *common.Context,
    scope: usize = 0,


    pub fn init(allocator: std.mem.Allocator, source: []const u8, ctx: *common.Context) @This() {
        var internal: HirNodeTypeBuilder() = undefined;
        inline for (comptime std.meta.fieldNames(HirNodeTypeBuilder())) |field| {
            @field(internal, field) = .empty;
        }
        const out = HirBuilder {
            .symbol_map = .init(allocator),
            .symbol_table = .empty,
            .allocator = allocator,
            .spans = .empty,
            .nodes = .empty,
            .source = source,
            .context = ctx,
            .internal_nodes = internal,
        };
        return out;
    }

    pub fn get(self: *const @This(), id: HirNodeId) struct {HirKind, *anyopaque} {
        const node = self.get_node(id);
        const node_index = node.index;
        const node_kind = node.kind;
        inline for (comptime std.meta.tags(HirKind)) |tag| {
            if (node_kind == tag) {
                return .{
                    tag,
                    @constCast(&@field(self.internal_nodes, @tagName(tag)).items[node_index]),
                };
            }
        }
        unreachable;
    }

    pub fn build(self: *@This()) !Hir {
        var out: Hir = undefined;

        inline for (comptime std.meta.fieldNames(HirNodeTypeBuilder())) |fl| {
            @field(out.internal_nodes, fl) = try @field(self.internal_nodes, fl).toOwnedSlice(self.allocator);
        }
        return out;
    }

    fn append(self: *@This(), array: anytype, data: anytype) !HirNodeId {
        try array.append(self.allocator, data);
        return array.items.len - 1;
    }

    pub fn add_node(self: *@This(), comptime kind: HirKind, span: common.Span, data: anytype) !HirNodeId {
        const id_name = @tagName(kind);
        const id = try self.append(&@field(self.internal_nodes, id_name), data);
        const spanid = try self.append(&self.spans, span);
        const out = try self.append(&self.nodes, HirNode {
            .kind = kind,
            .span = spanid,
            .index = id,
        });
        return out;
    }

    pub fn add_symbol(self: *@This(), span: common.Span) !SymbolId {
        const string = span.get_string(self.source);
        const intern = try self.context.intern_pool.put(string);
        const symbol = Symbol {
            .id = intern,
            .span = span,
        };
        var symbtab = self.symbol_table.items[self.scope];
        const symbolid = try self.append(&symbtab, symbol);
        try self.symbol_map.put(string, symbolid);
        return symbolid;
    }

    pub fn symbol_of(self: *const @This(), string: []const u8) ?SymbolId {
        const id = self.symbol_map.get(string);
        return id;
    }

    pub fn get_symbol(self: *const @This(), id: SymbolId) Symbol {
        return self.symbol_table.items[self.scope].items[id];
    }

    pub fn add_scope(self: *@This(), descend: bool) !ScopeId {
        const id = try self.append(&self.symbol_table, std.ArrayList(Symbol).empty);
        if (descend) {
            self.into_scope(id);
        }
        return id;
    }


    pub fn into_scope(self: *@This(), id: ScopeId) void {
        self.scope = id;
    }

    pub fn get_node(self: *const @This(), id: HirNodeId) HirNode {
        return self.nodes.items[id];
    }

    pub fn get_span(self: *const @This(), id: HirNodeId) common.Span {
        return self.spans.items[self.get_node(id).span];
    }

    pub fn is_poison(self: *const @This(), id: HirNodeId) bool {
        return self.get_node(id).kind == .poison;
    }



    pub fn deinit(self: *@This()) void {
        inline for (comptime std.meta.fieldNames(HirNodeTypeFinal())) |fl| {
            @field(self.internal_nodes, fl).deinit(self.allocator);
        }
    }

};

