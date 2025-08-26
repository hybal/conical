const std = @import("std");
const Ast = @import("Ast.zig");
const types = @import("types.zig");
const mem = @import("mem.zig");

pub const DefId = u64;
pub const HirId = u64;

pub const AdjustmentStep = union(enum) {
    AutoDeref,
    AutoRef,
    RefMutDiscard,
    MutDiscard,
    NumericCast: Ast.TypeId,
    PointerCast: Ast.TypeId,
};

pub const HirInfo = struct {
    ty: ?Ast.TypeId,
    adjustments: ?std.ArrayList(AdjustmentStep),
    span: types.Span,
    scope_id: usize,
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

    pub fn hash(self: *const @This()) u64 {
        return @intFromEnum(self.*);
    }
};

pub const UnaryOp = enum {
    Not,
    BinNot,
    DeRef,
    Ref,
    Minus,
    pub fn hash(self: *const @This()) u64 {
        return @intFromEnum(self.*);
    }
};

pub const BinaryExpr = struct {
    lhs: Hir,
    rhs: Hir,
    op: BinaryOp,

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(std.mem.asBytes(&self.op.hash()));
        hasher.update(std.mem.asBytes(&self.lhs.hash()));
        hasher.update(std.mem.asBytes(&self.rhs.hash()));
        return hasher.final();
    }
};

pub const Ident = struct {
    location: types.Span,
    value: []const u8,

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(std.mem.asBytes(&self.location.start));
        hasher.update(std.mem.asBytes(&self.location.end));
        hasher.update(self.value);
        return hasher.final();
    }
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

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        switch (self.*) {
            .string_literal => |v| {
                hasher.update(v);
            },
            .char_literal => |v| {
                hasher.update(std.mem.asBytes(&v));
            },
            .integer_literal => |v| {
                hasher.update(std.mem.asBytes(&v));
            },
            .float_literal => |v| {
                hasher.update(std.mem.asBytes(&v));
            },
            .identifier => |v| {
                hasher.update(std.mem.asBytes(&v));
            },
            .type_literal => |v| {
                hasher.update(std.mem.asBytes(&v.hash()));
            },
            .bool_literal => |v| {
                const val: u8 = if (v) 0 else 1;
                hasher.update(std.mem.asBytes(&val));
            },
            .unit => {
                hasher.update(std.mem.asBytes(&1));
            }
        }
        return hasher.final();
    }
};

pub const UnaryExpr = struct {
    op: UnaryOp,
    expr: Hir,

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(std.mem.asBytes(&self.op.hash()));
        hasher.update(std.mem.asBytes(&self.expr.hash()));
        return hasher.final();
    }
};

pub const Assignment = struct {
    lvalue: Hir,
    expr: Hir,

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(std.mem.asBytes(&self.lvalue.hash()));
        hasher.update(std.mem.asBytes(&self.expr.hash()));
        return hasher.final();
    }
};

pub const Binding = struct {
    id: DefId,
    ty: ?Ast.TypeId,
    is_mutable: bool,
    is_pub: bool,
    is_extern: bool,
    is_export: bool,
    is_static: bool,
    expr: Hir,

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(std.mem.asBytes(&self.id));
        if (self.ty) |ty| {
            hasher.update(std.mem.asBytes(&ty));
        }
        hasher.update(std.mem.asBytes(&self.is_mutable));
        hasher.update(std.mem.asBytes(&self.is_pub));
        hasher.update(std.mem.asBytes(&self.is_extern));
        hasher.update(std.mem.asBytes(&self.is_export));
        hasher.update(std.mem.asBytes(&self.is_static));
        hasher.update(std.mem.asBytes(&self.expr.hash()));
        return hasher.final();
    }
};

pub const TypeDecl = struct {
    id: DefId,
    is_pub: bool,
    is_extern: bool,
    tyid: Ast.TypeId,

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(std.mem.asBytes(&self.id));
        hasher.update(std.mem.asBytes(&self.is_pub));
        hasher.update(std.mem.asBytes(&self.is_extern));
        hasher.update(std.mem.asBytes(&self.tyid));
        return hasher.final();
    }
};

pub const EnumCons = struct {
    ty: Ast.TypeId,
    field: []const u8,
    value: ?Hir,

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(std.mem.asBytes(&self.ty));
        hasher.update(self.field);
        if (self.value) |value| {
            hasher.update(std.mem.asBytes(&value));
        }
        return hasher.final();
    }
};

pub const StructCons = struct {
    ty: Ast.TypeId,
    fields: std.StringHashMap(Hir),

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(std.mem.asBytes(&self.ty));
        var iter = self.fields.iterator();
        while (iter.next()) |next| {
            hasher.update(next.key_ptr.*);
            hasher.update(std.mem.asBytes(&next.value_ptr.hash()));
        }
        return hasher.final();
    }
};

pub const Block = struct {
    body: []Hir,

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        for (self.body) |stmt| {
            hasher.update(std.mem.asBytes(&stmt.hash()));
        }
        return hasher.final();
    }
};

pub const Loop = struct {
    block: Block,

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(std.mem.asBytes(&self.block.hash()));
        return hasher.final();
    }
};

pub const Fn = struct {
    id: DefId,
    parameters: []struct { id: Ident, ty: Ast.TypeId },
    return_type: Ast.TypeId,
    is_public: bool,
    is_extern: bool,
    is_export: bool,
    body: ?Hir,

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(std.mem.asBytes(&self.id));

        for (self.parameters) |param| {
            hasher.update(std.mem.asBytes(&param.id.hash()));
            hasher.update(std.mem.asBytes(&param.ty));
        }
        hasher.update(std.mem.asBytes(&self.return_type));
        hasher.update(std.mem.asBytes(&self.is_public));
        hasher.update(std.mem.asBytes(&self.is_extern));
        hasher.update(std.mem.asBytes(&self.is_export));
        if (self.body) |body| {
            hasher.update(std.mem.asBytes(&body.hash()));
        }
        return hasher.final();
    }
};

pub const FnCall = struct {
    expr: Hir,
    arguments: []Hir,

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(std.mem.asBytes(&self.expr.hash()));
        for (self.arguments) |arg| {
            hasher.update(std.mem.asBytes(&arg.hash()));
        }
        return hasher.final();
    }
};

pub const Branch = struct {
    a_path: Block,
    b_path: Block,
    condition: InlineExpr,

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(std.mem.asBytes(&self.a_path.hash()));
        hasher.update(std.mem.asBytes(&self.b_path.hash()));
        hasher.update(std.mem.asBytes(&self.condition.hash()));
        return hasher.final();
    }
};

pub const Cast = struct {
    expr: Hir,
    tyid: Ast.TypeId,

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(std.mem.asBytes(&self.expr.hash()));
        hasher.update(std.mem.asBytes(&self.tyid));
        return hasher.final();
    }
};



pub const Return = struct {
    expr: Hir,

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(std.mem.asBytes(&self.expr.hash()));
        return hasher.final();
    }
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

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();

        switch (self.*) {
            .binary_expr => |expr| hasher.update(std.mem.asBytes(&expr.hash())),
            .unary_expr => |expr| hasher.update(std.mem.asBytes(&expr.hash())),
            .terminal => |expr| hasher.update(std.mem.asBytes(&expr.hash())),
            .fn_call => |expr| hasher.update(std.mem.asBytes(&expr.hash())),
            .cast => |expr| hasher.update(std.mem.asBytes(&expr.hash())),
            .block => |expr| hasher.update(std.mem.asBytes(&expr.hash())),
            .enum_cons => |expr| hasher.update(std.mem.asBytes(&expr.hash())),
            .struct_cons => |expr| hasher.update(std.mem.asBytes(&expr.hash())),
        }
        return hasher.final();
    }
};

pub const TopLevelExpr = union(enum) {
    assignment: *Assignment,
    func: *Fn,
    return_stmt: *Return,
    loop: *Loop,
    branch: *Branch,
    binding: *Binding,
    type_decl: *TypeDecl,
    terminated: *Hir,

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        switch (self.*) {
            .assignment => |stmt| hasher.update(std.mem.asBytes(&stmt.hash())),
            .func => |stmt| hasher.update(std.mem.asBytes(&stmt.hash())),
            .return_stmt => |stmt| hasher.update(std.mem.asBytes(&stmt.hash())),
            .loop => |stmt| hasher.update(std.mem.asBytes(&stmt.hash())),
            .branch => |stmt| hasher.update(std.mem.asBytes(&stmt.hash())),
            .binding => |stmt| hasher.update(std.mem.asBytes(&stmt.hash())),
            .type_decl => |stmt| hasher.update(std.mem.asBytes(&stmt.hash())),
            .terminated => |stmt| hasher.update(std.mem.asBytes(&stmt.hash())),
        }
        return hasher.final();
    }
};


//This should always be trivially copiable other than the node.
pub const Hir = struct {
    node: HirNode,
    id: HirId,
    pub fn create(node: HirNode, scope_id: usize, span: types.Span, hir_table: *HirInfoTable) !@This() {
        const id = node.hash();
        try hir_table.put(id, .{
            .ty = null,
            .scope_id = scope_id,
            .adjustments = null,
            .span = span
        });
        return .{
            .node = node,
            .id = id,
        };
    }

    pub fn hash(self: *const @This()) u64 {
        return self.id;
    }
};

//This should always be pointer sized + the tag size
pub const HirNode = union(enum) {
    inline_expr: InlineExpr,
    top_level: TopLevelExpr,

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        switch (self.*) {
            .inline_expr => |expr| hasher.update(std.mem.asBytes(&expr.hash())),
            .top_level => |expr| hasher.update(std.mem.asBytes(&expr.hash())),
        }
        return hasher.final();
    }
};
