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


//represents an identifier,
//it has both a span and a value because it is included in the ast
//which may be shared among other compilation units which wont have access to the source that it came from
//FIXME: make sure that value is copied when it is created so that it isn't invalidated when the allocator is destroyed
pub const Ident = struct {
    span: common.Span,
    value: []const u8,

    pub fn equals(self: *const @This(), other: *const @This()) bool {
        if (self.span.start != other.span.start or self.span.end != other.span.end) {
            return false;
        }
        //technically checking "value" is redundent but is included for completness
        if (self.value.len != other.value.len) {
            return false;
        }
        for (self.value, 0..self.value.len) |val, i| {
            if (val != other.value[i]) {
                return false;
            }
        }
        return true;
    }

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(self.value);
        return hasher.final();
    }

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
    ident: Ident,
    ty: ?AstNodeId,
    is_mut: bool,
    initialize: ?AstNodeId
};

pub const GlobalDeclMod = enum {
    Pub,
    Extern,
    Export,
    PubExtern,
    PubExport,
};

pub const FnModifier = union(enum) {
    Pure,
    Async,
    CompTime
};
pub const FnDecl = struct {
    decl_mod: ?GlobalDeclMod,
    fn_mod: ?FnModifier,
    ident: Ident,
    params: []Ident, 
    param_types: []AstNodeId,
    return_ty: AstNodeId,
    body: ?AstNodeId,
};


pub const ModuleDecl = struct {
    path: AstNodeId,
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



pub const SpanId = usize;
pub const AstNodeId = usize;

pub const AstNode = struct {
    kind: AstKind,
    span: SpanId,
    index: usize,
};

// Is it bad that you have to change 8 things to add a new ast node, yes...
// Am I going to make it better? Maybe...
// Compile time stuff in zig aren't the easiest things to reason about.
pub const AstKind = enum {
    poison,
    unit,
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
    nodes: []const AstNode,
    poisons: []const Poison,
    spans: []const common.Span,
    units: []const Unit,
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
    module_decls: []const ModuleDecl,
    imports: []const Import,

    pub fn get(self: *@This(), id: AstNodeId) struct {AstKind, *anyopaque} {
        const node_index = self.nodes[id].index;
        const node_kind = self.nodes[id].kind;
        return switch (node_kind) {
            .poison => .{.poison, @constCast(&self.poisons[node_index])},
            .unit => .{.unit, @constCast(&self.units[node_index])},
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
            .module_decl => .{.module_decl, @constCast(&self.module_decls[node_index])},
            .import => .{.import, @constCast(&self.imports[node_index])},
        };
    }

    pub fn get_node(self: *@This(), id: AstNodeId) AstNode {
        return self.nodes[id];
    }

    pub fn get_span(self: *@This(), id: AstNodeId) common.Span {
        return self.spans[self.nodes[id].span];
    }
};

//NOTE: an optimization here would be to use ArrayListUnmanaged instead of ArrayList
// that way you only need to store the allocator once
pub const AstBuilder = struct {
    nodes: std.ArrayList(AstNode),
    poisons: std.ArrayList(Poison),
    spans: std.ArrayList(common.Span),
    units: std.ArrayList(Unit),
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
    module_decls: std.ArrayList(ModuleDecl),
    imports: std.ArrayList(Import),

    pub fn init(allocator: std.mem.Allocator) !AstBuilder {
        return .{
            .nodes = .init(allocator),
            .poisons = .init(allocator),
            .spans = .init(allocator),
            .units = .init(allocator),
            .binary_exprs = .init(allocator),
            .unary_exprs = .init(allocator),
            .terminals = .init(allocator),
            .type_exprs = .init(allocator),
            .type_binary_exprs = .init(allocator),
            .type_enums = .init(allocator),
            .type_structs = .init(allocator),
            .type_sets = .init(allocator),
            .type_int_ranges = .init(allocator),
            .assignments = .init(allocator),
            .if_stmts = .init(allocator),
            .while_loops = .init(allocator),
            .blocks = .init(allocator),
            .var_decls = .init(allocator),
            .fn_decls = .init(allocator),
            .fn_calls = .init(allocator),
            .return_stmts = .init(allocator),
            .type_decls = .init(allocator),
            .terminateds = .init(allocator),
            .intializer = .init(allocator),
            .access_operators = .init(allocator),
            .casts = .init(allocator),
            .paths = .init(allocator),
            .module_decls = .init(allocator),
            .imports = .init(allocator),
        };
    }
    fn append(comptime T: type, array: std.ArrayList(T), data: T) !usize {
        try array.append(data);
        return array.items.len - 1;
    }
    pub fn add_node(self: *@This(), 
        kind: AstKind,
        span: common.Span,
        data: anytype) !AstNodeId {
        const id = switch (kind) {
            .poison => try self.append(@TypeOf(self.poisons), self.poisons, data),
            .unit => try self.append(@TypeOf(self.units), self.units, data),
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
            .module_decl => try self.append(@TypeOf(self.module_decls), self.module_decls, data),
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
        const node_index = self.nodes[id].index;
        const node_kind = self.nodes[id].kind;
        return switch (node_kind) {
            .poison => .{.poison, @constCast(&self.poisons[node_index])},
            .unit => .{.unit, @constCast(&self.units[node_index])},
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
            .module_decl => .{.module_decl, @constCast(&self.module_decls[node_index])},
            .import => .{.import, @constCast(&self.imports[node_index])},
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
        return self.nodes[id];
    }

    pub fn get_span(self: *@This(), id: AstNodeId) common.Span {
        return self.spans[self.nodes[id].span];
    }

    pub fn build(self: *@This()) !Ast {
        return .{
            .nodes = try self.nodes.toOwnedSlice(),
            .poisons = try self.poisons.toOwnedSlice(),
            .spans = try self.spans.toOwnedSlice(),
            .units = try self.units.toOwnedSlice(),
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
            .module_decls = try self.module_decls.toOwnedSlice(),
            .imports = try self.imports.toOwnedSlice(),
        };
    }

};


