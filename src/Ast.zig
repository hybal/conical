const std = @import("std");
const mem = @import("mem.zig");
const types = @import("types.zig");


/// Represents a binary expression of the form: left op right
pub const BinaryExpr = struct {
    op: types.Token, //the operation
    left: AstNodeId, //left hand side of the operation
    right: AstNodeId, //right hand side of the operation
};

/// Represents a unary expression of the form: op expr
pub const UnaryExpr = struct {
    op: types.Token, //the operation
    expr: AstNodeId //the operation its applied to
};

/// Represents a field or method access, has the form: expr . ident
pub const AccessOperator = struct {
    left: AstNodeId,
    right: Ident,
};

/// Represents an assignment operaton including compound assignment operations like += or *=
pub const Assignment = struct {
    op: types.Token,
    lvalue: AstNodeId,
    expr: AstNodeId
};


//represents an identifier,
//it has both a span and a value because it is included in the ast
//which may be shared among other compilation units which wont have access to the source that it came from
//FIXME: make sure that value is copied when it is created so that it isn't invalidated when the allocator is destroyed
pub const Ident = struct {
    span: types.Span,
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
            if (!part.equals(other.parts[i])) {
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






pub const TypeCons = struct {
    ty: AstNodeId,
    fields: std.StringHashMap(?AstNodeId),
};

pub const TypeDecl = struct {
    ty: AstNodeId,
    ident: Ident,
    //Eventually will have constraints here
    //This will also actually be an expression since you can do weird comptime things
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



const SpanId = usize;
const AstNodeId = usize;

pub const AstNode = struct {
    kind: AstKind,
    span: SpanId,
    index: usize,
};

pub const AstKind = enum {
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
    type_cons,
    access_operator,
    cast,
    path,
    module_decl,
    import,
};

pub const AstBuilder = struct {
    nodes: std.ArrayList(AstNode),
    spans: std.ArrayList(types.Span),
    binary_exprs: std.ArrayList(BinaryExpr),
    unary_exprs: std.ArrayList(UnaryExpr),
    terminals: std.ArrayList(types.Token),
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
    type_cons: std.ArrayList(TypeCons),
    access_operators: std.ArrayList(AccessOperator),
    casts: std.ArrayList(Cast),
    paths: std.ArrayList(Path),
    module_decls: std.ArrayList(ModuleDecl),
    imports: std.ArrayList(Import),

    pub fn init(allocator: std.mem.Allocator) !AstBuilder {
        return .{
            .nodes = .init(allocator),
            .spans = .init(allocator),
            .binary_exprs = .init(allocator),
            .unary_expr = .init(allocator),
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
            .type_cons = .init(allocator),
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
        span: types.Span,
        data: anytype) !AstNodeId {
        const id = switch (kind) {
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
            .type_cons => try self.append(@TypeOf(self.type_cons), self.type_cons, data),
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

    pub fn build(self: *@This()) !Ast {
        return .{
            .nodes = try self.nodes.toOwnedSlice(),
            .spans = try self.spans.toOwnedSlice(),
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
            .type_cons = try self.type_cons.toOwnedSlice(),
            .access_operators = try self.access_operators.toOwnedSlice(),
            .casts = try self.casts.toOwnedSlice(),
            .paths = try self.paths.toOwnedSlice(),
            .module_decls = try self.module_decls.toOwnedSlice(),
            .imports = try self.imports.toOwnedSlice(),
        };
    }

};

pub const Ast = struct {
    nodes: []AstNode,
    spans: []types.Span,
    binary_exprs: []BinaryExpr,
    unary_exprs: []UnaryExpr,
    terminals: []types.Token,
    type_exprs: []TypeExpr,
    type_binary_exprs: []TypeBinaryExpr,
    type_enums: []TypeEnum,
    type_structs: []TypeStruct,
    type_sets: []TypeSet,
    type_int_ranges: []TypeIntRange,
    assignments: []Assignment,
    if_stmts: []IfStmt,
    while_loops: []WhileLoop,
    blocks: []Block,
    var_decls: []VarDecl,
    fn_decls: []FnDecl,
    fn_calls: []ParamList,
    return_stmts: []ReturnStmt,
    type_decls: []TypeDecl,
    terminateds: []Terminated,
    type_cons: []TypeCons,
    access_operators: []AccessOperator,
    casts: []Cast,
    paths: []Path,
    module_decls: []ModuleDecl,
    imports: []Import,

    pub fn get(self: *@This(), id: AstNodeId) *anyopaque {
        const node_index = self.nodes[id].index;
        const node_kind = self.nodes[id].kind;
        return switch (node_kind) {
            .binary_expr => &self.binary_exprs[node_index],
            .unary_expr => &self.unary_exprs[node_index],
            .terminal => &self.terminals[node_index],
            .type_expr => &self.type_exprs[node_index],
            .type_binary_expr => &self.type_binary_exprs[node_index],
            .type_enum => &self.type_enums[node_index],
            .type_struct => &self.type_structs[node_index],
            .type_set => &self.type_sets[node_index],
            .type_int_range => &self.type_int_ranges[node_index],
            .assignment => &self.assignments[node_index],
            .if_stmt => &self.if_stmts[node_index],
            .while_loop => &self.while_loops[node_index],
            .block => &self.blocks[node_index],
            .var_decl => &self.var_decls[node_index],
            .fn_decl => &self.fn_decls[node_index],
            .fn_call => &self.fn_calls[node_index],
            .return_stmt => &self.return_stmts[node_index],
            .type_decl => &self.type_decls[node_index],
            .terminated => &self.terminateds[node_index],
            .type_cons => &self.type_cons[node_index],
            .access_operator => &self.access_operators[node_index],
            .cast => &self.casts[node_index],
            .path => &self.paths[node_index],
            .module_decl => &self.module_decls[node_index],
            .import => &self.imports[node_index],
        };
    }

    pub fn get_node(self: *@This(), id: AstNodeId) AstNode {
        return self.nodes[id];
    }

    pub fn get_span(self: *@This(), id: AstNodeId) types.Span {
        return self.spans[self.nodes[id].span];
    }
};
