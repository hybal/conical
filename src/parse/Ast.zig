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

/// Represents a field or method access, has the form: expr.(ident | integer)
pub const AccessOperator = struct {
    left: AstNodeId,
    right: Token,
};

pub const SliceOp = struct {
    expr: AstNodeId,
    left: AstNodeId,
    right: ?AstNodeId,
};

pub const IndexOp = struct {
    expr: AstNodeId,
    index: AstNodeId,
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

pub const TerminalType = union(enum) {
    value: Token,
    symbol: Ident,
};

pub const Terminal = struct {
    span: common.Span,
    termtype: TerminalType,
};

pub const InitializerField = struct {
    id: ?Ident,
    value: AstNodeId,
};
pub const Initializer = struct {
    ty: ?AstNodeId,
    fields: []InitializerField,
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

pub const BindingModifierKind = enum {
    alias,
    mut,
    move,
};
pub const BindingModifier = struct {
    span: common.Span,
    kind: BindingModifierKind,
};

pub const FnDecl = struct {
    ident: Ident,
    params: []BindingId, 
    param_types: []AstNodeId,
    generics: []Generic,
    return_ty: ?AstNodeId,
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

pub const FnArg = struct {
    id: ?Ident,
    val: AstNodeId,
    is_generic: bool,
};
pub const FnCall = struct {
    left: AstNodeId,
    params: []FnArg,
};

pub const LambdaParam = struct {
    mod: ?BindingModifier,
    ident: Ident,
    ty: ?AstNodeId,
    //default: ?AstNodeId,
};

pub const Lambda = struct {
    params: []LambdaParam,
    generics: []Generic,
    ret_ty: ?AstNodeId,
    expr: AstNodeId,
};

pub const Refinement = struct {
    a: Ident,
    b: ?Ident,
};
pub const IfStmt = struct {
    condition: AstNodeId,
    block: AstNodeId,
    else_block: ?AstNodeId,
    refinements: ?[]Refinement,
};

pub const Match = struct {
    arms: []AstNodeId,
    expr: AstNodeId,
};

pub const MatchArm = struct {
    patterns: ?[]AstNodeId,
    captures: ?[]Ident,
    block: AstNodeId,
};

pub const Pattern = union(enum) {
    literal: AstNodeId,
    range: AstNodeId,
    compound: AstNodeId,
    catch_all,
};

pub const MatchCompoundLiteral = struct {
    ids: []Ident,
    values: []AstNodeId,
};

pub const LoopControlType = enum {
    @"continue",
    @"break",
};

pub const LoopControl = struct {
    control: LoopControlType,
};

pub const Loop = struct {
    block: AstNodeId,
};

pub const ForLoop = struct {
    ident: Ident,
    block: AstNodeId,
    expr: AstNodeId,
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
    match,
    match_arm,
    match_pattern,
    match_compound_literal,
    while_loop,
    for_loop,
    loop,
    loop_control,
    block,
    var_decl,
    fn_decl,
    fn_call,
    lambda,
    return_stmt,
    type_decl,
    terminated,
    initializer,
    access_operator,
    index,
    slice,
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
    matchs: []const Match,
    match_arms: []const MatchArm,
    match_patterns: []const Pattern,
    match_compound_literals: []const MatchCompoundLiteral,
    while_loops: []const WhileLoop,
    for_loops: []const ForLoop,
    loops: []const Loop,
    loop_controls: []const LoopControl,
    blocks: []const Block,
    var_decls: []const VarDecl,
    fn_decls: []const FnDecl,
    fn_calls: []const FnCall,
    lambdas: []const Lambda,
    return_stmts: []const ReturnStmt,
    type_decls: []const TypeDecl,
    terminateds: []const Terminated,
    initializers: []const Initializer,
    access_operators: []const AccessOperator,
    indexs: []const IndexOp,
    slices: []const SliceOp,
    casts: []const Cast,
    paths: []const Path,
    imports: []const Import,

    pub fn get(self: *const @This(), id: AstNodeId) struct {AstKind, *anyopaque} {
        const node_index = self.nodes[id].index;
        const node_kind = self.nodes[id].kind;
        inline for (std.meta.tags(AstKind)) |tag| {
            if (node_kind == tag) {
                const field_name = comptime @tagName(tag) ++ "s";

                comptime {
                    if (!@hasField(@This(), field_name)) {
                        @compileError("Missing field " ++ field_name);
                    }
                }

                return .{
                    tag,
                    @constCast(&@field(self, field_name)[node_index]),
                };
            }
        }
    }

    pub fn get_node(self: *const @This(), id: AstNodeId) AstNode {
        return self.nodes[id];
    }

    pub fn get_span(self: *const @This(), id: AstNodeId) common.Span {
        return self.spans[self.nodes[id].span];
    }

    pub fn get_program(self: *const @This()) Program {
        return self.program;
    }

    pub fn deinit(self: *const @This(), allocator: std.mem.Allocator) void {
        inline for (comptime std.meta.fieldNames(Ast)) |fl| {
            if ((comptime std.mem.eql(u8, fl, "program"))) {
                allocator.free(self.program.declarations);
            } else {
                allocator.free(@field(self, fl));
            }
        }
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

        pub fn deinit(self: *@This()) void {
            inline for(comptime std.meta.fieldNames(Base)) |fl| {
                if (!comptime std.mem.eql(u8, fl, "program")) {
                    @field(self.Self, fl).deinit(self.allocator);
                }
            }
        }

        fn append(self: *@This(), array: anytype, data: anytype) !usize {
            try array.append(self.allocator, data);
            return array.items.len - 1;
        }

        pub fn add_node(self: *@This(),
            comptime kind: AstKind,
            span: common.Span,
            data: anytype) !AstNodeId {
            const id_name = @tagName(kind) ++ "s";
            const id = try self.append(&@field(self.Self, id_name), data);
            const spanid = try self.append(&self.Self.spans, span);

            const out = try self.append(&self.Self.nodes, AstNode {
                .kind = kind,
                .span = spanid,
                .index = id,
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
                        @constCast(&@field(self.Self, field_name).items[node_index]),
                    };
                }

            }
        }

        pub fn build(self: *@This()) !Ast {
            var out: Ast = undefined;

            inline for (comptime std.meta.fieldNames(Base)) |fl| {
                if (!comptime std.mem.eql(u8, fl, "program")) {
                    @field(out, fl) = try @field(self.Self, fl).toOwnedSlice(self.allocator);
                } else {
                    if (self.Self.program == null) {
                        return error.UnsetFieldProgram;
                    }
                    out.program = self.Self.program.?;
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
            return self.Self.nodes.items.len > id and self.Self.nodes.items[id].kind == .poison;
        }

    };
}

pub const AstBuilder = _AstBuilder();

