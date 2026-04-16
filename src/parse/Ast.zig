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
    Intersection,
    Equality,
    Subset,
    StrictSubset,
    SuperSet,
    StrictSuperSet,
    Membership
};

pub const TypeModifierOp = union(enum) {
    Reference,
    Slice,
    Array: AstNodeId,
};

pub const TypeModifier = struct {
    mods: []TypeModifierOp,
    expr: AstNodeId,
};

pub const TypeMetadata = struct {
    left: AstNodeId,
    right: AstNodeId,
    op: TypeMetadataOp,
};

pub const TypeMetadataOp = enum {
    associative,
};

pub const TypeBinaryExpr = struct {
    left: AstNodeId,
    right: AstNodeId,
    op: TypeBinaryOp,
};

pub const TypeRange = struct {
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

pub const TypeImpl = struct {
    declarations: []AstNodeId,
};

pub const TypeLiteral = union(enum) {
    value: Token,
    self,
    symbol: Ident,
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
    type_metadata,
    type_literal,
    type_modifier,
    type_label,
    type_enum,
    type_struct,
    type_impl,
    type_set,
    type_range,
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
    type_modifiers: []const TypeModifier,
    type_metadatas: []const TypeMetadata,
    type_labels: []const TypeLabel,
    type_literals: []const TypeLiteral,
    type_enums: []const TypeEnum,
    type_structs: []const TypeStruct,
    type_impls: []const TypeImpl,
    type_sets: []const TypeSet,
    type_ranges: []const TypeRange,
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
    intializers: []const Initializer,
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
            .type_metadata => .{.type_metadata, @constCast(&self.type_metadatas[node_index])},
            .type_modifier => .{.type_modifier, @constCast(&self.type_modifiers[node_index])},
            .type_label => .{.type_label, @constCast(&self.type_labels[node_index])},
            .type_enum => .{.type_enum, @constCast(&self.type_enums[node_index])},
            .type_struct => .{.type_struct, @constCast(&self.type_structs[node_index])},
            .type_impl => .{.type_impl, @constCast(&self.type_impls[node_index])},
            .type_set => .{.type_set, @constCast(&self.type_sets[node_index])},
            .type_range => .{.type_range, @constCast(&self.type_ranges[node_index])},
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
            .intializer => .{.intializer, @constCast(&self.intializers[node_index])},
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

fn TransformFieldType(comptime T: type) type {
    return switch(@typeInfo(T)) {
        .pointer => |ptr| {
            if (ptr.size == .slice) {
                return std.ArrayList(ptr.child);
            }
            return T;
        },
        else => T,
    };
}

fn TransformAst(comptime S: type) type {
    const info = @typeInfo(S).@"struct";

    var new_fields_names: [info.fields.len][]const u8 = undefined;
    var new_fields_types: [info.fields.len]type = undefined;

    inline for (info.fields, 0..) |field, i| {
        new_fields_names[i] = field.name;
        if (std.mem.eql(u8, field.name, "program")) {
            new_fields_types[i] = ?Program;
        } else {
            new_fields_types[i] = TransformFieldType(field.@"type");
        }
    }

    return @Struct(
        .auto,
        null,
        &new_fields_names,
        &new_fields_types,
        &@splat(.{}),
    );
}

// Comptime in zig is kind of strange, but this essentially creates an inner type of every field in Ast + 's' and also translates all of the slices to std.ArrayList types.
fn _AstBuilder() type {
    const Base = TransformAst(Ast);

    return struct {
        Self: Base,
        allocator: std.mem.Allocator,
        pub fn init(allocator: std.mem.Allocator) @This() {
            var out: Base = undefined;
            inline for (@typeInfo(Base).@"struct".fields) |field| {
                if (comptime std.mem.eql(u8, field.name, "program")) {
                    @field(out, field.name) = null;
                } else {
                    @field(out, field.name) = .empty;
                }
            }
            return .{ .Self = out, .allocator = allocator, };
        }


        pub fn add_node(self: *@This(),
            comptime kind: AstKind,
            span: common.Span,
            data: anytype) !AstNodeId {
            const id_name = @tagName(kind) ++ "s";
            const id = try self.append(@TypeOf(@field(self.Self, id_name)), @field(self.Self, id_name), data);
            const spanid = try self.append(@TypeOf(self.Self.spans), self.Self.spans, span);

            const out = try self.append(AstNode, self.Self.nodes, .{
                .kind = kind,
                .spanid = spanid,
                .id = id,
            });
            return out;
        }
        
        pub fn get(self: *@This(), id: AstNodeId) struct { AstKind, *anyopaque } {
            const node = self.Self.nodes.items[id];
            const node_index = node.index;
            const node_kind = node.kind;

            inline for (std.meta.tags(AstKind)) |tag| {
                @compileLog("Added: " ++ @tagName(tag) ++ "s");
                if (node_kind == tag) {
                    const field_name = comptime @tagName(tag) ++ "s";

                    comptime {
                        if (!@hasField(@This().Self, field_name)) {
                            @compileError("Missing field" ++ field_name);
                        }
                    }

                    return .{
                        tag,
                        @constCast(&@field(self, field_name).items[node_index]),
                    };
                }

            }
        }

        pub fn build(self: *@This()) !Ast {
            var out: Ast = undefined;

            inline for (std.meta.fieldNames(@This().Self)) |fl| {
                if (!comptime std.mem.eql(u8, fl, "program")) {
                    @field(out, fl) = try @field(self.Self, fl).toOwnedSlice();
                } else {
                    if (self.program == null) {
                        return error.UnsetFieldProgram;
                    }
                    out.program = self.program.?;
                }

            }

            return out;

        }

        pub fn get_or_null(self: *@This(), T: type, kind: AstKind, id: AstNodeId) ?T {
            const node = self.Self.get(id);
            if (node.@"0" == kind) {
                return @as(T, @constCast(node.@"1"));
            }
            return null;
        }

        pub fn get_node(self: *@This(), id: AstNodeId) AstNode {
            return self.Self.nodes.items[id];
        }

        pub fn get_span(self: *@This(), id: AstNodeId) common.Span {
            return self.Self.spans.items[self.Self.nodes.items[id].span];
        }

        pub fn set_program(self: *@This(), prog: Program) void {
            self.Self.program = prog;
        }

        pub fn get_program(self: *@This()) ?Program {
            return self.Self.program;
        }

        pub fn is_poison(self: *const @This(), id: AstNodeId) bool {
            return self.Self.nodes.items[id].kind == .poison;
        }

    };
}

pub const AstBuilder = _AstBuilder();

