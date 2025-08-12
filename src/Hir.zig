const std = @import("std");
const Ast = @import("Ast.zig");
const types = @import("types.zig");
const mem = @import("mem.zig");

pub const DefId = usize;

pub const HirId = struct {
    owner: usize,
    local: usize,

    pub fn eql(a: @This(), b: @This()) bool {
        return a.owner == b.owner and a.local == b.local;
    }

    pub fn hash(id: @This()) u64 {
        return @intCast(id.owner * 31 + id.local);
    }
};

pub const AdjustmentStep = union(enum) {
    AutoDeref: usize,
    AutoRef: usize,
    FunctionPointerCoercion,
    ClosureFnPointerCoercion,
    NumericCast: Ast.TypeId,
    PointerCast: Ast.TypeId,
};

pub const HirInfo = struct {
    ty: ?Ast.TypeId,
    adjustments: ?std.ArrayList(AdjustmentStep),
    span: types.Span,
};

pub const HirInfoTable = std.AutoHashMap(HirId, HirInfo);

pub const BinaryOp = enum {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
    BinAnd,
    BinOr,
    BinXor,
    LAnd,
    LOr,
    Eq,
    NotEq,
    Gt,
    Lt,
    GtEq,
    LtEq,
    Dot,
    Cast,
};

pub const UnaryOp = enum {
    Not,
    BinNot,
    DeRef,
    Ref,
    Minus,
};

pub const BinaryExpr = struct {
    lhs: Hir,
    rhs: Hir,
    op: BinaryOp,
};

pub const Ident = struct {
    location: types.Span,
    value: []const u8,
};

pub const Terminal = union(enum) {
    string_literal: []const u8, 
    char_literal: i32,
    integer_literal: u128,
    float_literal: f64, 
    identifier: DefId, 
    type_literal: *Ast.Type,
    bool_literal: bool,
    unit,
};

pub const UnaryExpr = struct {
    op: UnaryOp,
    expr: Hir,
};

pub const Assignment = struct {
    lvalue: Hir,
    expr: Hir,
};

pub const Binding = struct {
    id: Ident,
    ty: ?Ast.TypeId,
    is_mutable: bool,
    is_pub: bool,
    is_extern: bool,
    is_export: bool,
    is_static: bool,
    expr: Hir,
};

pub const TypeDecl = struct {
    id: Ident,
    is_pub: bool,
    is_extern: bool,
    tyid: Ast.TypeId,
};

pub const EnumCons = struct {
    ty: Ast.TypeId,
    field: []const u8,
    value: ?*Ast.Ast,
};

pub const StructCons = struct {
    ty: Ast.TypeId,
    fields: std.StringHashMap(Hir),
};

pub const Block = struct {
    body: []Hir,
};

pub const Loop = struct {
    block: Block,
};

pub const Fn = struct {
    id: Ident,
    parameters: []struct { id: Ident, ty: Ast.TypeId },
    return_type: Ast.TypeId,
    is_public: bool,
    is_extern: bool,
    is_export: bool,
    body: ?Hir,
};

pub const FnCall = struct {
    expr: Hir,
    arguments: []Hir
};

pub const Branch = struct {
    a_path: Block,
    b_path: Block,
    condition: InlineExpr,
};

pub const Cast = struct {
    expr: Hir,
    tyid: Ast.TypeId
};

pub const Return = struct {
    expr: Hir,
};

pub const InlineExpr = union(enum) {
    binary_expr: *BinaryExpr,
    unary_expr: *UnaryExpr,
    terminal: *Terminal,
    fn_call: *FnCall,
    cast: *Cast,
    block: *Block,
    enum_cons: *EnumCons,
    struct_cons: *StructCons,
};

pub const TopLevelExpr = union(enum) {
    assignment: *Assignment,
    func: *Fn,
    return_stmt: *Return,
    loop: *Loop,
    branch: *Branch,
    binding: *Binding,
    type_decl: *TypeDecl,
};

pub const HirIdGenerator = struct {
    allocator: std.mem.Allocator,
    local_counters: std.AutoHashMap(usize, usize),
    current_owner: usize,
    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .allocator = allocator,
            .local_counters = .init(allocator),
            .current_owner = 0
        };
    }

    pub fn next(self: *@This()) !HirId {
        _ = try self.local_counters.getOrPutValue(self.current_owner, 0);
        const next_local = self.local_counters.get(self.current_owner).?;
        const out: HirId = .{ .owner = self.current_owner, .local = next_local };
        try self.local_counters.put(self.current_owner, next_local + 1);
        return out;
    }

    pub fn push_owner(self: *@This()) void {
        self.current_owner += 1;
    }

    pub fn pop_owner(self: *@This()) usize {
        defer self.current_owner -= 1;
        return self.current_owner;
    }

    pub fn set_owner(self: *@This(), owner: usize) void {
        self.current_owner = owner;
    }
};

//This should always be trivially copiable other than the node.
pub const Hir = struct {
    node: HirNode,
    id: HirId,
    pub fn create(node: HirNode, span: types.Span, hir_table: *HirInfoTable, id_gen: *HirIdGenerator) !@This() {
        const id = try id_gen.next();
        try hir_table.put(id, .{
            .ty = null,
            .adjustments = null,
            .span = span
        });
        return .{
            .node = node,
            .id = id,
        };
    }
};

//This should always be pointer sized + the tag size
pub const HirNode = union(enum) {
    inline_expr: InlineExpr,
    top_level: TopLevelExpr
};
