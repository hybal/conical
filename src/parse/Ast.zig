const std = @import("std");
const common = @import("common");
const Token = @import("lex").Token;
const ErrorId = @import("diagnostics").ErrorId;


/// Represents a binary expression of the form: left op right
pub const BinaryExpr = struct {
    op: Token, //the operation
    left: AstNodeId, //left hand side of the operation
    right: AstNodeId, //right hand side of the operation
};

/// Represents a unary expression of the form: op expr
pub const UnaryExpr = struct {
    op: Token, //the operation
    expr: AstNodeId //the operation its applied to
};

/// Represents a field or method access, has the form: expr . ident
pub const AccessOperator = struct {
    left: AstNodeId,
    right: Ident,
};

/// Represents an assignment operaton including compound assignment operations like += or *=
pub const Assignment = struct {
    op: Token,
    lvalue: AstNodeId,
    expr: AstNodeId
};


pub const Ident = struct {
    span: common.Span,
};

pub const BindingId = struct {
    id: Ident,
    modifier: ?BindingModifier,
};


pub const TypeExpr = struct {
    exprs: []AstNodeId,
};

pub const TypeBinaryOp = enum {
    Union,
    Product,
    Difference,
    Equality,
    Subset,
    StrictSubset,
    SuperSet,
    StrictSuperSet,
    Inclusion
};

pub const TypeBinaryExpr = struct {
    left: AstNodeId,
    right: AstNodeId,
    op: TypeBinaryOp,
};

pub const TypeIntRange = struct {
    start: AstNodeId,
    start_inclusive: bool,
    end: AstNodeId,
    end_inclusive: bool,
};

pub const TypeSet = struct {
    values: []AstNodeId,
};

pub const TypeLabel = struct {
    label: Ident,
    expr: AstNodeId,
};

pub const TypeStruct = struct {
    field_labels: []Ident,
    field_exprs: []AstNodeId,
};

pub const TypeEnum = struct {
    variants: []AstNodeId,
};


pub const Path = struct {
    parts: []Ident,

    pub fn equals(self: *const @This(), other: *const @This()) bool {
        if (self.parts.len != other.parts.len) {
            return false;
        }
        for (self.parts, 0..) |part, i| {
            if (!part.equals(&other.parts[i])) {
                return false;
            }
        }
        return true;
    }


    pub fn get_string(self: *const @This(), gpa: std.mem.Allocator) ![]const u8 {
        var out = std.ArrayList(u8).init(gpa);
        if (self.parts.len == 1) {
            try out.appendSlice(self.parts[0].value);
        } else {
            for (self.parts) |part| {
                try out.appendSlice(part.value);
                try out.appendSlice("::");
            }
        }
        return try out.toOwnedSlice();
    }

    pub fn hash(self: *const @This()) !u64 {
        var hasher = std.hash.Fnv1a_64.init();
        for (self.parts) |part| {
            hasher.update(part.value);
        }
        return hasher.final();
    }

};

pub const Terminal = struct {
    tok: Token,
};

pub const InitializerField = struct {
    id: Ident,
    value: AstNodeId,
};
pub const Initializer = struct {
    ty: ?AstNodeId,
    fields: []InitializerField,
    //expr: ?AstNodeId,
};

pub const TypeDecl = struct {
    ty: AstNodeId,
    ident: Ident,
};


//a variable decleration ast node
pub const VarDecl = struct {
    id: BindingId,
    ty: ?AstNodeId,
    initialize: AstNodeId
};

pub const BindingModifier = struct {
    span: common.Span,
    kind: enum {
        alias,
        mut,
        move,
    },
};

pub const FnDecl = struct {
    ident: Ident,
    params: []BindingId, 
    param_types: []AstNodeId,
    generics: []Generic,
    return_ty: AstNodeId,
    body: ?AstNodeId,
};

pub const Generic = struct {
    ident: Ident,
    expr: ?AstNodeId,
};
pub const FnModKind = union(enum) {
    pure,
    @"inline",
    @"comptime",
};

pub const FnMod = struct {
    kind: FnModKind,
    span: common.Span,
};

pub const Visibility = struct {
    span: common.Span,
    kind: enum {
        public,
    },
};

pub const Linkage = struct {
    span: common.Span,
    kind: enum {
        @"extern",
        @"export",
    },
};

pub const Item = struct {
    visibility: ?Visibility,
    linkage: ?Linkage,
    function_mods: ?[]FnMod,
    item_kind: ItemKind,
    item: AstNodeId,
};

pub const ItemKind = enum {
    function,
    @"type",
    binding,
};

pub const Import = struct {
    path: AstNodeId,
};

pub const ReturnStmt = struct {
    expr: AstNodeId,
};

pub const Terminated = struct {
    expr: AstNodeId,
};

pub const ParamList = struct {
    left: AstNodeId,
    params: []AstNodeId,
    //add generics here
};

pub const IfStmt = struct {
    condition: AstNodeId,
    block: AstNodeId,
    else_block: ?AstNodeId
};

pub const WhileLoop = struct {
    condition: AstNodeId,
    block: AstNodeId
};
pub const Cast = struct {
    expr: AstNodeId,
    ty: AstNodeId,
};
pub const Ternary = struct {
    condition: AstNodeId,
    true_path: AstNodeId,
    false_path: AstNodeId
};

pub const Block = struct {
    exprs: []AstNodeId
};

pub const Poison = struct {
    error_id: ErrorId,
};

pub const Unit = struct {};

pub const ModuleDecl = struct {
    path: AstNodeId,
    span: common.Span,
};
pub const Program = struct {
    module: ?ModuleDecl,
    declarations: []AstNodeId,
};


pub const SpanId = usize;
pub const AstNodeId = usize;

pub const AstNode = struct {
    kind: AstKind,
    span: SpanId,
    index: usize,
};

// Is it bad that you have to change 8 things to add a new ast node, yes...
// Am I going to make it better? Maybe...
// Compile time stuff in zig isn't the easiest thing to reason about.
pub const AstKind = enum {
    poison,
    unit,
    item,
    binary_expr,
    unary_expr,
    terminal,
    type_expr,
    type_binary_expr,
    type_enum,
    type_struct,
    type_set,
    type_int_range,
    assignment,
    if_stmt,
    while_loop,
    block,
    var_decl,
    fn_decl,
    fn_call,
    return_stmt,
    type_decl,
    terminated,
    intializer,
    access_operator,
    cast,
    path,
    module_decl,
    import,
};

pub const Ast = struct {
    program: Program,
    nodes: []const AstNode,
    poisons: []const Poison,
    spans: []const common.Span,
    units: []const Unit,
    items: []const Item,
    binary_exprs: []const BinaryExpr,
    unary_exprs: []const UnaryExpr,
    terminals: []const Terminal,
    type_exprs: []const TypeExpr,
    type_binary_exprs: []const TypeBinaryExpr,
    type_enums: []const TypeEnum,
    type_structs: []const TypeStruct,
    type_sets: []const TypeSet,
    type_int_ranges: []const TypeIntRange,
    assignments: []const Assignment,
    if_stmts: []const IfStmt,
    while_loops: []const WhileLoop,
    blocks: []const Block,
    var_decls: []const VarDecl,
    fn_decls: []const FnDecl,
    fn_calls: []const ParamList,
    return_stmts: []const ReturnStmt,
    type_decls: []const TypeDecl,
    terminateds: []const Terminated,
    intializer: []const Initializer,
    access_operators: []const AccessOperator,
    casts: []const Cast,
    paths: []const Path,
    imports: []const Import,

    pub fn get(self: *@This(), id: AstNodeId) struct {AstKind, *anyopaque} {
        const node_index = self.nodes[id].index;
        const node_kind = self.nodes[id].kind;
        return switch (node_kind) {
            .poison => .{.poison, @constCast(&self.poisons[node_index])},
            .unit => .{.unit, @constCast(&self.units[node_index])},
            .item => .{.item, @constCast(&self.items[node_index])},
            .binary_expr => .{.binary_expr, @constCast(&self.binary_exprs[node_index])},
            .unary_expr => .{.unary_expr, @constCast(&self.unary_exprs[node_index])},
            .terminal => .{.terminal, @constCast(&self.terminals[node_index])},
            .type_expr => .{.type_expr, @constCast(&self.type_exprs[node_index])},
            .type_binary_expr => .{.type_binary_expr, @constCast(&self.type_binary_exprs[node_index])},
            .type_enum => .{.type_enum, @constCast(&self.type_enums[node_index])},
            .type_struct => .{.type_struct, @constCast(&self.type_structs[node_index])},
            .type_set => .{.type_set, @constCast(&self.type_sets[node_index])},
            .type_int_range => .{.type_int_range, @constCast(&self.type_int_ranges[node_index])},
            .assignment => .{.assignment, @constCast(&self.assignments[node_index])},
            .if_stmt => .{.if_stmt, @constCast(&self.if_stmts[node_index])},
            .while_loop => .{.while_loop, @constCast(&self.while_loops[node_index])},
            .block => .{.block, @constCast(&self.blocks[node_index])},
            .var_decl => .{.var_decl, @constCast(&self.var_decls[node_index])},
            .fn_decl => .{.fn_decl, @constCast(&self.fn_decls[node_index])},
            .fn_call => .{.fn_call, @constCast(&self.fn_calls[node_index])},
            .return_stmt => .{.return_stmt, @constCast(&self.return_stmts[node_index])},
            .type_decl => .{.type_decl, @constCast(&self.type_decls[node_index])},
            .terminated => .{.terminated, @constCast(&self.terminateds[node_index])},
            .intializer => .{.intializer, @constCast(&self.intializer[node_index])},
            .access_operator => .{.access_operator, @constCast(&self.access_operators[node_index])},
            .cast => .{.cast, @constCast(&self.casts[node_index])},
            .path => .{.path, @constCast(&self.paths[node_index])},
            .import => .{.import, @constCast(&self.imports[node_index])},
        };
    }

    pub fn get_node(self: *@This(), id: AstNodeId) AstNode {
        return self.nodes[id];
    }

    pub fn get_span(self: *@This(), id: AstNodeId) common.Span {
        return self.spans[self.nodes[id].span];
    }

    pub fn get_program(self: *@This()) Program {
        return self.program;
    }
};

pub const AstBuilder = struct {
    allocator: std.mem.Allocator,
    program: ?Program,
    nodes: std.ArrayList(AstNode),
    poisons: std.ArrayList(Poison),
    spans: std.ArrayList(common.Span),
    units: std.ArrayList(Unit),
    items: std.ArrayList(Item),
    binary_exprs: std.ArrayList(BinaryExpr),
    unary_exprs: std.ArrayList(UnaryExpr),
    terminals: std.ArrayList(Terminal),
    type_exprs: std.ArrayList(TypeExpr),
    type_binary_exprs: std.ArrayList(TypeBinaryExpr),
    type_enums: std.ArrayList(TypeEnum),
    type_structs: std.ArrayList(TypeStruct),
    type_sets: std.ArrayList(TypeSet),
    type_int_ranges: std.ArrayList(TypeIntRange),
    assignments: std.ArrayList(Assignment),
    if_stmts: std.ArrayList(IfStmt),
    while_loops: std.ArrayList(WhileLoop),
    blocks: std.ArrayList(Block),
    var_decls: std.ArrayList(VarDecl),
    fn_decls: std.ArrayList(FnDecl),
    fn_calls: std.ArrayList(ParamList),
    return_stmts: std.ArrayList(ReturnStmt),
    type_decls: std.ArrayList(TypeDecl),
    terminateds: std.ArrayList(Terminated),
    intializer: std.ArrayList(Initializer),
    access_operators: std.ArrayList(AccessOperator),
    casts: std.ArrayList(Cast),
    paths: std.ArrayList(Path),
    imports: std.ArrayList(Import),

    pub fn init(allocator: std.mem.Allocator) AstBuilder {
        return .{
            .allocator = allocator,
            .program = null,
            .nodes = .empty,
            .poisons = .empty,
            .spans = .empty,
            .units = .empty,
            .items = .empty,
            .binary_exprs = .empty,
            .unary_exprs = .empty,
            .terminals = .empty,
            .type_exprs = .empty,
            .type_binary_exprs = .empty,
            .type_enums = .empty,
            .type_structs = .empty,
            .type_sets = .empty,
            .type_int_ranges = .empty,
            .assignments = .empty,
            .if_stmts = .empty,
            .while_loops = .empty,
            .blocks = .empty,
            .var_decls = .empty,
            .fn_decls = .empty,
            .fn_calls = .empty,
            .return_stmts = .empty,
            .type_decls = .empty,
            .terminateds = .empty,
            .intializer = .empty,
            .access_operators = .empty,
            .casts = .empty,
            .paths = .empty,
            .imports = .empty,
        };
    }
    fn append(self: *@This(), comptime T: type, array: std.ArrayList(T), data: T) !usize {
        try array.append(self.allocator, data);
        return array.items.len - 1;
    }
    pub fn add_node(self: *@This(), 
        kind: AstKind,
        span: common.Span,
        data: anytype) !AstNodeId {
        const id = switch (kind) {
            .poison => try self.append(@TypeOf(self.poisons), self.poisons, data),
            .unit => try self.append(@TypeOf(self.units), self.units, data),
            .item => try self.append(@TypeOf(self.items), self.items, data),
            .binary_expr => try self.append(@TypeOf(self.binary_exprs), self.binary_exprs, data),
            .unary_expr => try self.append(@TypeOf(self.unary_exprs), self.unary_exprs, data),
            .terminal => try self.append(@TypeOf(self.terminals), self.terminals, data),
            .type_expr => try self.append(@TypeOf(self.type_exprs), self.type_exprs, data),
            .type_binary_expr => try self.append(@TypeOf(self.type_binary_exprs), self.type_binary_exprs, data),
            .type_enum => try self.append(@TypeOf(self.type_enums), self.type_enums, data),
            .type_struct => try self.append(@TypeOf(self.type_structs), self.type_structs, data),
            .type_set => try self.append(@TypeOf(self.type_sets), self.type_sets, data),
            .type_int_range => try self.append(@TypeOf(self.type_int_ranges), self.type_int_ranges, data),
            .assignment => try self.append(@TypeOf(self.assignments), self.assignments, data),
            .if_stmt => try self.append(@TypeOf(self.if_stmts), self.if_stmts, data),
            .while_loop => try self.append(@TypeOf(self.while_loops), self.while_loops, data),
            .block => try self.append(@TypeOf(self.blocks), self.blocks, data),
            .var_decl => try self.append(@TypeOf(self.var_decls), self.var_decls, data),
            .fn_decl => try self.append(@TypeOf(self.fn_decls), self.fn_decls, data),
            .fn_call => try self.append(@TypeOf(self.fn_calls), self.fn_calls, data),
            .return_stmt => try self.append(@TypeOf(self.return_stmts), self.return_stmts, data),
            .type_decl => try self.append(@TypeOf(self.type_decls), self.type_decls, data),
            .terminated => try self.append(@TypeOf(self.terminateds), self.terminateds, data),
            .intializer => try self.append(@TypeOf(self.intializer), self.intializer, data),
            .access_operator => try self.append(@TypeOf(self.access_operators), self.access_operators, data),
            .cast => try self.append(@TypeOf(self.casts), self.casts, data),
            .path => try self.append(@TypeOf(self.paths), self.paths, data),
            .import => try self.append(@TypeOf(self.imports), self.imports, data),
        };
        const spanid = try self.append(@TypeOf(self.spans), self.spans, span);
        const out = try self.append(AstNode, self.nodes, .{
            .kind = kind,
            .spanid = spanid,
            .index = id,
        });
        return out;
    }
    pub fn get(self: *@This(), id: AstNodeId) struct {AstKind, *anyopaque} {
        const node_index = self.nodes.items[id].index;
        const node_kind = self.nodes.items[id].kind;
        return switch (node_kind) {
            .poison => .{.poison, @constCast(&self.poisons.items[node_index])},
            .unit => .{.unit, @constCast(&self.units.items[node_index])},
            .item => .{.item, @constCast(&self.items.items[node_index])},
            .binary_expr => .{.binary_expr, @constCast(&self.binary_exprs.items[node_index])},
            .unary_expr => .{.unary_expr, @constCast(&self.unary_exprs.items[node_index])},
            .terminal => .{.terminal, @constCast(&self.terminals.items[node_index])},
            .type_expr => .{.type_expr, @constCast(&self.type_exprs.items[node_index])},
            .type_binary_expr => .{.type_binary_expr, @constCast(&self.type_binary_exprs.items[node_index])},
            .type_enum => .{.type_enum, @constCast(&self.type_enums.items[node_index])},
            .type_struct => .{.type_struct, @constCast(&self.type_structs.items[node_index])},
            .type_set => .{.type_set, @constCast(&self.type_sets.items[node_index])},
            .type_int_range => .{.type_int_range, @constCast(&self.type_int_ranges.items[node_index])},
            .assignment => .{.assignment, @constCast(&self.assignments.items[node_index])},
            .if_stmt => .{.if_stmt, @constCast(&self.if_stmts.items[node_index])},
            .while_loop => .{.while_loop, @constCast(&self.while_loops.items[node_index])},
            .block => .{.block, @constCast(&self.blocks.items[node_index])},
            .var_decl => .{.var_decl, @constCast(&self.var_decls.items[node_index])},
            .fn_decl => .{.fn_decl, @constCast(&self.fn_decls.items[node_index])},
            .fn_call => .{.fn_call, @constCast(&self.fn_calls.items[node_index])},
            .return_stmt => .{.return_stmt, @constCast(&self.return_stmts.items[node_index])},
            .type_decl => .{.type_decl, @constCast(&self.type_decls.items[node_index])},
            .terminated => .{.terminated, @constCast(&self.terminateds.items[node_index])},
            .intializer => .{.intializer, @constCast(&self.intializer.items[node_index])},
            .access_operator => .{.access_operator, @constCast(&self.access_operators.items[node_index])},
            .cast => .{.cast, @constCast(&self.casts.items[node_index])},
            .path => .{.path, @constCast(&self.paths.items[node_index])},
            .import => .{.import, @constCast(&self.imports.items[node_index])},
        };
    }

    pub fn get_or_null(self: *@This(), T: type, kind: AstKind, id: AstNodeId) ?T {
        const node = self.get(id);
        if (node.@"0" == kind) {
            return @as(T, @constCast(node.@"1"));
        }
        return null;
    }

    pub fn get_node(self: *@This(), id: AstNodeId) AstNode {
        return self.nodes.items[id];
    }

    pub fn get_span(self: *@This(), id: AstNodeId) common.Span {
        return self.spans.items[self.nodes.items[id].span];
    }

    pub fn set_program(self: *@This(), prog: Program) void {
        self.program = prog;
    }

    pub fn get_program(self: *@This()) ?Program {
        return self.program;
    }

    pub fn is_poison(self: *const @This(), id: AstNodeId) bool {
        return self.nodes.items[id].kind == .poison;
    }

    pub fn build(self: *@This()) !Ast {
        return .{
            .nodes = try self.nodes.toOwnedSlice(),
            .poisons = try self.poisons.toOwnedSlice(),
            .spans = try self.spans.toOwnedSlice(),
            .units = try self.units.toOwnedSlice(),
            .items = try self.items.toOwnedSlice(),
            .binary_exprs = try self.binary_exprs.toOwnedSlice(),
            .unary_exprs = try self.unary_exprs.toOwnedSlice(),
            .terminals = try self.terminals.toOwnedSlice(),
            .type_exprs = try self.type_exprs.toOwnedSlice(),
            .type_binary_exprs = try self.type_binary_exprs.toOwnedSlice(),
            .type_enums = try self.type_enums.toOwnedSlice(),
            .type_structs = try self.type_structs.toOwnedSlice(),
            .type_sets = try self.type_sets.toOwnedSlice(),
            .type_int_ranges = try self.type_int_ranges.toOwnedSlice(),
            .assignments = try self.assignments.toOwnedSlice(),
            .if_stmts = try self.if_stmts.toOwnedSlice(),
            .while_loops = try self.while_loops.toOwnedSlice(),
            .blocks = try self.blocks.toOwnedSlice(),
            .var_decls = try self.var_decls.toOwnedSlice(),
            .fn_decls = try self.fn_decls.toOwnedSlice(),
            .fn_calls = try self.fn_calls.toOwnedSlice(),
            .return_stmts = try self.return_stmts.toOwnedSlice(),
            .type_decls = try self.type_decls.toOwnedSlice(),
            .terminateds = try self.terminateds.toOwnedSlice(),
            .intializer = try self.intializer.toOwnedSlice(),
            .access_operators = try self.access_operators.toOwnedSlice(),
            .casts = try self.casts.toOwnedSlice(),
            .paths = try self.paths.toOwnedSlice(),
            .imports = try self.imports.toOwnedSlice(),
        };
    }

};


