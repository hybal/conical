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

pub const Unwrap = struct {
    expr: AstNodeId,
};

/// Represents an assignment operaton including compound assignment operations like += or *=
pub const Assignment = struct {
    op: Token,
    lvalue: AstNodeId,
    expr: AstNodeId
};


pub const Ident = struct {
    span: common.Either(common.Span, ErrorId),
};

pub const BindingId = struct {
    id: Ident,
    modifier: ?BindingModifier,
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

/// In the future this can contain linear/affine/distinct prefix types
pub const TypePrefixKind = enum {
    relevant,
};

pub const TypePrefix = struct {
    kind: TypePrefixKind,
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

pub const TypeDefault = struct {
    left: AstNodeId,
    right: AstNodeId,
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

pub const TypeInterface = struct {
    decls: []AstNodeId,
};

pub const TypeUnaryExprKind = enum {
    negate,
};

pub const TypeUnaryExpr = struct {
    kind: TypeUnaryExprKind,
    expr: AstNodeId,
};

pub const TypeLiteral = union(enum) {
    value: Token,
    symbol: Ident,
    self,
    @"type",
    discard,
};

pub const TypeUseMod = struct {
    kind: TypeUseModKind,
    expr: AstNodeId,
};

pub const TypeUseModKind = enum {
    relevant,
};


pub const TerminalType = union(enum) {
    value: Token,
    symbol: Ident,
};

pub const Terminal = struct {
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

pub const FnParam = struct {
    id: BindingId,
    mod: ?BindingModifier,
    ty: ?AstNodeId,
    is_generic: bool,
};

pub const FnDecl = struct {
    ident: Ident,
    params: []FnParam,
    return_ty: ?AstNodeId,
    body: ?AstNodeId,
};

pub const Generic = struct {
    ident: Ident,
    expr: ?AstNodeId,
};
pub const EvalModifier = enum {
    pure,
    @"inline",
    @"comptime",
};

pub const FnMod = struct {
    kind: EvalModifier,
    span: common.Span,
};

pub const Linkage = struct {
    span: common.Span,
    kind: enum {
        @"extern",
        @"export",
    },
};

pub const ModStmt = struct {
    expression: AstNodeId,
};

pub const Item = struct {
    linkage: ?Linkage,
    item_kind: ItemKind,
    item: AstNodeId,
};

pub const ItemKind = enum {
    function,
    binding,
    mod_expr,
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
};

pub const Lambda = struct {
    params: []LambdaParam,
    ret_ty: ?AstNodeId,
    expr: AstNodeId,
};

pub const Attribute = struct {
    id: Ident,
    args: ?[]AstNodeId
};

pub const AttributeList = struct {
    attributes: []Attribute,
    value: AstNodeId,
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
    pattern: AstNodeId,
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
    exprs: []AstNodeId,
};

pub const ModBlock = struct {
    block: AstNodeId,
    mod: EvalModifier,
};

pub const Poison = struct {
    error_id: ErrorId,
};

pub const Unit = struct {};

pub const Program = struct {
    items: []AstNodeId,
};

pub const SpanId = usize;
pub const AstNodeId = usize;

pub const AstNode = struct {
    kind: AstKind,
    span: SpanId,
    index: usize,
};

pub const AstKind = enum {
    poison,
    unit,
    item,
    mod_stmt,
    binary_expr,
    unary_expr,
    terminal,
    type_binary_expr,
    type_unary_expr,
    type_metadata,
    type_literal,
    type_modifier,
    type_prefix,
    type_label,
    type_default,
    type_enum,
    type_struct,
    type_impl,
    type_interface,
    type_set,
    type_range,
    type_use_mod,
    assignment,
    match,
    match_arm,
    match_pattern,
    match_compound_literal,
    while_loop,
    for_loop,
    loop,
    loop_control,
    block,
    mod_block,
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
    unwrap,
    slice,
    import,
    attribute_list,
};

pub const Ast = struct {
    program: Program,
    node: []const AstNode,
    poison: []const Poison,
    span: []const common.Span,
    unit: []const Unit,
    item: []const Item,
    mod_stmt: []const ModStmt,
    binary_expr: []const BinaryExpr,
    unary_expr: []const UnaryExpr,
    terminal: []const Terminal,
    type_binary_expr: []const TypeBinaryExpr,
    type_unary_expr: []const TypeUnaryExpr,
    type_modifier: []const TypeModifier,
    type_prefix: []const TypePrefix,
    type_metadata: []const TypeMetadata,
    type_label: []const TypeLabel,
    type_default: []const TypeDefault,
    type_literal: []const TypeLiteral,
    type_enum: []const TypeEnum,
    type_struct: []const TypeStruct,
    type_impl: []const TypeImpl,
    type_interface: []const TypeInterface,
    type_set: []const TypeSet,
    type_range: []const TypeRange,
    assignment: []const Assignment,
    match: []const Match,
    match_arm: []const MatchArm,
    match_pattern: []const Pattern,
    match_compound_literal: []const MatchCompoundLiteral,
    while_loop: []const WhileLoop,
    for_loop: []const ForLoop,
    loop: []const Loop,
    loop_control: []const LoopControl,
    block: []const Block,
    mod_block: []const ModBlock,
    var_decl: []const VarDecl,
    fn_decl: []const FnDecl,
    fn_call: []const FnCall,
    lambda: []const Lambda,
    return_stmt: []const ReturnStmt,
    type_decl: []const TypeDecl,
    terminated: []const Terminated,
    initializer: []const Initializer,
    access_operator: []const AccessOperator,
    index: []const IndexOp,
    unwrap: []const Unwrap,
    slice: []const SliceOp,
    import: []const Import,
    type_use_mod: []const TypeUseMod,
    attribute_list: []const AttributeList,

    pub fn get(self: *const @This(), id: AstNodeId) struct {AstKind, *anyopaque} {
        const node_index = self.node[id].index;
        const node_kind = self.node[id].kind;
        inline for (comptime std.meta.tags(AstKind)) |tag| {
            if (node_kind == tag) {
                const field_name = comptime @tagName(tag);

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
        unreachable;
    }

    pub fn get_node(self: *const @This(), id: AstNodeId) AstNode {
        return self.node[id];
    }

    pub fn get_span(self: *const @This(), id: AstNodeId) common.Span {
        return self.span[self.node[id].span];
    }

    pub fn get_program(self: *const @This()) Program {
        return self.program;
    }

    pub fn deinit(self: *const @This(), allocator: std.mem.Allocator) void {
        inline for (comptime std.meta.fieldNames(Ast)) |fl| {
            if ((comptime std.mem.eql(u8, fl, "program"))) {
                allocator.free(self.program.expressions);
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
            const id_name = @tagName(kind);
            const id = try self.append(&@field(self.Self, id_name), data);
            const spanid = try self.append(&self.Self.span, span);

            const out = try self.append(&self.Self.node, AstNode {
                .kind = kind,
                .span = spanid,
                .index = id,
            });
            return out;
        }
        
        pub fn get(self: *@This(), id: AstNodeId) struct { AstKind, *anyopaque } {
            const node = self.Self.node.items[id];
            const node_index = node.index;
            const node_kind = node.kind;

            inline for (comptime std.meta.tags(AstKind)) |tag| {
                if (node_kind == tag) {
                    const field_name = comptime @tagName(tag);

                    comptime {
                        if (!@hasField(Base, field_name)) {
                            @compileError("Missing field" ++ field_name);
                        }
                    }

                    return .{
                        tag,
                        @constCast(&@field(self.Self, field_name).items[node_index]),
                    };
                }
            }
            unreachable;
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

        pub fn get_or_null(self: *@This(), T: type, kind: AstKind, id: AstNodeId) ?*T {
            const node = self.get(id);
            if (node.@"0" == kind) {
                return @as(*T, @alignCast(@ptrCast(node.@"1")));
            }
            return null;
        }

        pub fn get_node(self: *@This(), id: AstNodeId) AstNode {
            return self.Self.node.items[id];
        }

        pub fn get_span(self: *@This(), id: AstNodeId) common.Span {
            return self.Self.span.items[self.Self.node.items[id].span];
        }

        pub fn set_program(self: *@This(), prog: Program) void {
            self.Self.program = prog;
        }

        pub fn get_program(self: *@This()) ?Program {
            return self.Self.program;
        }

        pub fn is_poison(self: *const @This(), id: AstNodeId) bool {
            return self.Self.node.items.len > id and self.Self.nodes.items[id].kind == .poison;
        }

    };
}

pub const AstBuilder = _AstBuilder();

