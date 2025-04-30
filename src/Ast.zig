const std = @import("std");
const mem = @import("mem.zig");
const types = @import("types.zig");

/// Represents a binary expression of the form: left op right
pub const BinaryExpr = struct {
    op: types.Token, //the operation
    left: *Ast, //left hand side of the operation
    right: *Ast, //right hand side of the operation
};

/// Represents a unary expression of the form: op expr
pub const UnaryExpr = struct {
    op: types.Token, //the operation
    expr: *Ast //the operation its applied to
};



/// Represents an assignment operaton including compound assignment operations like += or *=
pub const Assignment = struct {
    op: types.Token,
    lvalue: *Ast,
    expr: *Ast
};

/// Represents a builtin primitive type
pub const PrimitiveType = enum {
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,
    U8,
    U16,
    U32,
    U64,
    U128,
    USize,
    F32,
    F64,
    Bool,
    Str,
    Rune, //may not be needed
    Unit,
    pub fn get_string(self: *const @This()) []const u8 {
        return switch (self.*) {
            .I8 => "i8",
            .I16 => "i16",
            .I32 => "i32",
            .I64 => "i64",
            .I128 => "i128",
            .ISize => "isize",
            .U8 => "u8",
            .U16 => "u16",
            .U32 => "u32",
            .U64 => "u64",
            .U128 => "u128",
            .USize => "usize",
            .F32 => "f32",
            .F64 => "f64",
            .Bool => "bool",
            .Str => "str",
            .Rune => "rune",
            .Unit => "()",
        };
    }
    pub const prims = std.StaticStringMap(@This()).initComptime(.{
        .{ "i8", .I8 },
        .{ "i16", .I16 },
        .{ "i32", .I32 },
        .{ "i64", .I64 },
        .{ "i128", .I128 },
        .{ "isize", .ISize },
        .{ "u8", .U8 },
        .{ "u16", .U16 },
        .{ "u32", .U32 },
        .{ "u64", .U64 },
        .{ "u128", .U128 },
        .{ "usize", .USize },
        .{ "f32", .F32 },
        .{ "f64", .F64 },
        .{ "bool", .Bool },
        .{ "str", .Str },
        .{ "rune", .Rune },
        .{ "()", .Unit },
    });
    pub fn from_string(str: []const u8) ?@This() {
        return prims.get(str);
    }


    pub fn hash(self: *const @This()) u64 {
        return @intFromEnum(self.*);
    }
};

pub const TypeModifier = union(enum) {
    Array: types.Token,
    Slice,
    Ref,
    Vector,
    Comptime,
    Mut,
    Const,
    RefMut,
    RefConst,
    Ptr,
    PtrMut,
    PtrConst,
    
    pub fn get_string(self: *const TypeModifier, source: []const u8) []const u8 {
        switch (self.*) {
            .Array => |tok| return tok.span.get_string(source),
            .Slice => return "[]",
            .Ref => return "&",
            .Mut => return "mut",
            .Const => return "const",
            .RefMut => return "&mut",
            .RefConst => return "&const",
            .Ptr => return "*",
            .PtrMut => return "*mut",
            .PtrConst => return "*const",
            .Comptime => return "comp",
            else => unreachable
        }
    }
    pub fn equal(self: *const @This(), other: @This()) bool {
        if (self.Array) |sarr| {
            if (other.Array) |oarr| {
                return sarr.tag == oarr.tag;
            }
            return true;
        }
        return self == other;
    }

    pub fn hash(self: *const @This()) u64 {
        switch (self.*) {
            .Array => |val| {
                //IMPORTANT: Make sure to keep this up to date with the last enum varient
                return @intFromEnum(@This().PtrConst) + @intFromEnum(val.tag);
            },
            else => {
                return @intFromEnum(self.*);
            }
        }
    }
};

pub const Ident = struct {
    span: types.Span,
    value: []const u8
};

pub const FuncType = struct {
    args: []Type,
    ret: *Type, //This is only a pointer because zig complains

    
    pub fn hash(self: *const @This(), hasher: *std.hash.Fnv1a_64) void {
        hasher.update(std.mem.asBytes(&self.ret.*.hash()));
        for (self.args) |arg| {
            hasher.update(std.mem.asBytes(&(arg.hash())));
        }
    }
};

pub const Type = struct {
    base_type: union(enum) {
        primitive: PrimitiveType,
        func: FuncType,
        user: Ident
    },
    modifiers: ?[]TypeModifier,
    chash: ?u64 = null,

    pub fn hash(self: *const @This()) u64 {
        if (self.chash) |hsh| {
            return hsh;
        }
        var hasher = std.hash.Fnv1a_64.init();
        switch (self.base_type) {
            .primitive => |prim| {
                hasher.update(std.mem.asBytes(&(prim.hash())));
            },
            .func => |func| {
                func.hash(&hasher);
            },
            .user => |ident| {
                hasher.update(ident.value);
            }
        }
        if (self.modifiers) |mods| {
            for (mods) |mod| {
                hasher.update(std.mem.asBytes(&mod.hash()));
            }
        }

        return hasher.final();
    }

    pub fn equal(self: *const @This(), other: *const @This()) bool {
        return self.hash() == other.hash();
    }

    pub fn createPrimitive(prim: PrimitiveType, modifiers: ?[]TypeModifier) @This() {
        return .{
            .base_type = .{
                .primitive = prim,
            },
            .modifiers = modifiers
        };
    }

};


pub const VarDecl = struct {
    ident: Ident,
    ty: ?TypeId,
    is_mut: bool,
    initialize: ?*Ast
};

pub const GlobalDeclMod = enum {
    Pub,
    Extern,
    Export,
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
    params: []Ident, //TODO: move parameter list into a list of VarDecl
    param_types: []TypeId,
    return_ty: TypeId,
    body: ?*Ast
};

pub const FnCall = struct {
    func: *Ast,
    args: []*Ast
};


pub const Struct = struct {
    ident: Ident,
    fields: std.StringHashMap(Type)
};


pub const Enum = struct {
    ident: Ident,
    varients: std.StringHashMap(?Type)
};

pub const IfStmt = struct {
    condition: *Ast,
    block: *Ast,
    else_block: ?*Ast
};

pub const WhileLoop = struct {
    condition: *Ast,
    block: *Ast
};

pub const Ternary = struct {
    condition: *Ast,
    true_path: *Ast,
    false_path: *Ast
};

pub const Block = struct {
    exprs: []*Ast
};

pub const TypeId = u64;

pub const Ast = struct {
    node: AstNode,
    span: types.Span,
    pub fn create(node: AstNode, span: types.Span) @This() {
        return .{
            .node = node,
            .span = span
        };
    }
};

pub const AstNode = union(enum) { 
    binary_expr: BinaryExpr,
    unary_expr: UnaryExpr,
    terminal: types.Token,
    assignment: Assignment, 
    if_stmt: IfStmt,
    while_loop: WhileLoop,
    ternary: Ternary,
    block: Block,
    var_decl: VarDecl,
    fn_decl: FnDecl,
    fn_call: FnCall,
    terminated: *Ast,
    _,
};
