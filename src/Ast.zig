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

/// Represents a field or method access, has the form: expr . ident
pub const AccessOperator = struct {
    left: *Ast,
    right: Ident,
};

/// Represents an assignment operaton including compound assignment operations like += or *=
pub const Assignment = struct {
    op: types.Token,
    lvalue: *Ast,
    expr: *Ast
};

/// Represents a builtin primitive type
pub const PrimitiveType = enum {
    //signed integers from 8-128 bits
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize, //platform dependent signed integer
    //unsigned integers from 8-128 bits
    U8,
    U16,
    U32,
    U64,
    U128,
    USize, //platform dependent unsigned integer
    F32,
    F64,
    Bool,
    Rune, //may not be needed
    //the type of an expression that is never evaluated/completed
    //it is the type of the return statement and any other control flow statements
    Never, 
    //represents a 'nothing' value, it is both a type and its value
    //anything that does not return a value returns unit
    //it is NOT equivalent to null as it is generally optimized out
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
            .Rune => "rune",
            .Never => "!",
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
        .{ "rune", .Rune },
        .{ "()", .Unit },
    });
    pub fn from_string(str: []const u8) ?@This() {
        return prims.get(str);
    }

    //NOTE: This must be kept up to date with the last variant
    pub fn get_last() @This() {
        return .Unit;
    }
    pub fn hash(self: *const @This()) u64 {
        return @intFromEnum(self.*);
    }
    fn pow(a: comptime_int, b: comptime_int) comptime_int {
        var out: comptime_int = 0;
        for (0..b) |_| {
            out += a;
        }
        return out;
    }
    pub fn from_int(val: u128, signed: bool) @This() {
        return switch (val) {
            0...pow(2, 8) - 1 => if (signed) .I8 else .U8,
            pow(2, 8)...pow(2, 16) - 1 => if (signed) .I16 else .U16,
            pow(2, 16)...pow(2, 32) - 1 => if (signed) .I32 else .U32,
            pow(2, 32)...pow(2, 64) - 1 => if (signed) .I64 else .U64,
            else => if (signed) .I128 else .U128,
        };
    }

    pub fn is_signed_int(self: @This()) bool {
        return switch (self) {
            .I8,
            .I16,
            .I32,
            .I64,
            .I128,
            .ISize => true,
            else => false
        };
    }

    pub fn is_unsigned_int(self: @This()) bool {
        return switch (self) {
            .U8,
            .U16,
            .U32,
            .U64,
            .U128,
            .USize => true,
            else => false
        };
    }
    pub fn is_int(self: @This()) bool {
        return self.is_signed_int() or self.is_unsigned_int();
    }

    pub fn switch_sign(self: @This()) @This() {
        if (!self.is_int()) return self;
        return switch (self) {
            .I8 => .U8,
            .I16 => .U16,
            .I32 => .U32,
            .I64 => .U64,
            .I128 => .U128,
            .ISize => .USize,
            .U8 => .I8,
            .U16 => .I16,
            .U32 => .I32,
            .U64 => .I64,
            .U128 => .I128,
            .USize => .ISize,
            else => unreachable
        };
    }

    pub fn is_float(self: @This()) bool {
        return self == .F32 or self == .F64;
    }


    pub fn get_bits(self: @This(), platform_size: u8) u8 {
        return switch(self) {
            .I8, .U8 => 8,
            .I16, .U16 => 16,
            .I32, .U32, .F32 => 32,
            .I64, .U64, .F64 => 64,
            .I128, .U128 => 128,
            .ISize, .USize => platform_size,
            .Rune => 32,
            .Bool => 1,
            .Never => 0,
            .Unit => 0
        };
    }

    pub fn equals(self: *const @This(), other: *const @This()) bool {
        return self.* == other.*;
    }

};

//Various type modifiers such as references and slices
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
    pub fn equals(self: @This(), other: @This()) bool {
        if (self == .Array) {
            if (other == .Array) {
                return self.Array.tag == other.Array.tag;
            }
            return true;
        }
        return @intFromEnum(self) == @intFromEnum(other);
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

//represents an identifier,
//it has both a span and a value because it is included in the ast
//which may be shared among other compilation units which wont have access to the source that it came from
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

    pub fn get_base(self: *const @This()) Ident {
        return self.parts[self.parts.len - 1];
    }

    pub fn get_string(self: *const @This(), gpa: std.mem.Allocator) ![]const u8 {
        var out = std.ArrayList(u8).init(gpa);
        for (self.parts) |part| {
            try out.appendSlice(part.value);
            try out.appendSlice("::");
        }
        return try out.toOwnedSlice();
    }
};

//represents a function type
pub const FuncType = struct {
    args: []TypeId,
    ret: TypeId, 

    pub fn get_string(self: *const @This(), type_map: *types.TypeTbl, gpa: std.mem.Allocator, source: []const u8) anyerror![]const u8 {
        var out = std.ArrayList(u8).init(gpa);
        try out.appendSlice("fn (");
        for (self.args, 0..) |arg, i| {
            try out.appendSlice(try type_map.get(arg).?.get_string(type_map, gpa, source));
            if (i < self.args.len - 1) {
                try out.appendSlice(", ");
            }
        }
        try out.appendSlice(") -> ");
        try out.appendSlice(try type_map.get(self.ret).?.get_string(type_map, gpa, source));
        return try out.toOwnedSlice();
    }
    pub fn hash(self: *const @This(), hasher: *std.hash.Fnv1a_64) void {
        hasher.update(std.mem.asBytes(&self.ret));
        for (self.args) |arg| {
            hasher.update(std.mem.asBytes(&arg));
        }
    }

    pub fn equals(self: *const @This(), other: *const @This()) bool {
        if (self.args.len != other.args.len) return false;
        if (self.ret != other.ret) return false;
        for (self.args, 0..self.args.len) |arg, i| {
            if (arg != other.args[i]) {
                return false;
            }
        }
        return true;
    }
};

pub const BaseType = union(enum) { //what the underlying type is
    primitive: PrimitiveType,
    func: FuncType,
    strct: Struct,
    @"enum": Enum,
    user: Ident,
    @"type": TypeId,

    pub fn equals(self: *const @This(), other: *const @This()) bool {
        if (@intFromEnum(self.*) != @intFromEnum(other.*)) {
            return false;
        }

        return switch (self.*) {
            .primitive => self.primitive.equals(&other.primitive),
            .func => self.func.equals(&other.func),
            .strct => self.strct.equals(&other.strct),
            .@"enum" => self.@"enum".equals(&other.@"enum"),
            .user => self.user.equals(&other.user),
            .@"type" => self.@"type" == other.@"type",
        };
    }

};


//the overall type representation
pub const Type = struct {
    base_type: BaseType,
    modifiers: ?[]TypeModifier, //the modifiers for this type
    chash: ?u64 = null, //not really used, but is intended to cache the hash of this type

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
            .strct => |strct| {
                strct.hash(&hasher);
            },
            .@"enum" => |en| {
                en.hash(&hasher);
            },
            .user => |ident| {
                hasher.update(ident.value);
            },
            .@"type" => |ty| {
                hasher.update(std.mem.asBytes(&ty));
            }
        }
        if (self.modifiers) |mods| {
            for (mods) |mod| {
                hasher.update(std.mem.asBytes(&mod.hash()));
            }
        }

        return hasher.final();
    }

    pub fn get_string(self: *const @This(), type_map: *types.TypeTbl, gpa: std.mem.Allocator, source: []const u8) ![]const u8 {
        var out = std.ArrayList(u8).init(gpa);
        if (self.modifiers) |mods| {
            for (mods) |mod| {
                try out.appendSlice(mod.get_string(source));
            }
        }

        switch (self.base_type) {
            .primitive => |val| try out.appendSlice(val.get_string()),
            .func => |func| try out.appendSlice(try func.get_string(type_map, gpa, source)),
            .strct => |strct| try out.appendSlice(try strct.get_string(gpa, type_map, source)),
            .@"enum" => |enm| try out.appendSlice(try enm.get_string(gpa, type_map, source)),
            .user => |val| try out.appendSlice(val.value),
            .@"type" => |ty| try out.appendSlice(try type_map.get(ty).?.get_string(type_map, gpa, source))
        }
        return try out.toOwnedSlice();
    }

    pub fn equals(self: *const @This(), other: *const @This()) bool {
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

    pub fn is_signed_int(self: *const @This()) bool {
        return self.base_type == .primitive and self.base_type.primitive.is_signed_int();
    }
    pub fn is_unsigned_int(self: *const @This()) bool {
        return self.base_type == .primitive and self.base_type.primitive.is_unsigned_int();
    }

    pub fn is_int(self: *const @This()) bool {
        return self.is_signed_int() or self.is_unsigned_int();
    }

    pub fn is_float(self: *const @This()) bool {
        return self.base_type == .primitive and self.base_type.primitive.is_float();
    }

    pub fn is_never(self: *const @This()) bool {
        return self.base_type == .primitive and self.base_type.primitive == .Never;
    }

};

//a struct ast node
pub const Struct = struct {
    fields: std.StringHashMap(TypeId),

    pub fn hash(self: *const @This(), hasher: *std.hash.Fnv1a_64) void {
        var iterator = self.fields.valueIterator();
        while (iterator.next()) |val| {
            hasher.update(std.mem.asBytes(&val));
        }
    }

    pub fn get_string(self: *const @This(), gpa: std.mem.Allocator, type_map: *types.TypeTbl, source: []const u8) anyerror![]const u8 {
        var out = std.ArrayList(u8).init(gpa);
        try out.appendSlice("struct { ");
        var iter = self.fields.iterator();
        while (iter.next()) |val| {
            try out.appendSlice(val.key_ptr.*);
            try out.appendSlice(": ");
            try out.appendSlice(try (type_map.get(val.value_ptr.*).?.get_string(type_map, gpa, source)));
        }
        return try out.toOwnedSlice();
    }

    pub fn equals(self: *const @This(), other: *const @This()) bool {
        if (self.fields.count() != other.fields.count()) {
            return false;
        }

        var iterator = self.fields.iterator();
        while (iterator.next()) |next| {
            if (other.fields.get(next.key_ptr.*)) |val| {
                if (val != next.value_ptr.*) {
                    return false;
                }
            } else {
                return false;
            }
        }
        return true;
    }
};


pub const TypeCons = struct {
    ty: TypeId,
    fields: std.StringHashMap(?*Ast),
};

pub const TypeDecl = struct {
    ty: TypeId,
    ident: Ident,
    //Eventually will have constraints here
    //This will also actually be an expression since you can do weird comptime things
};

pub const Enum = struct { //TODO:finalize syntax
    variants: std.StringHashMap(?TypeId),

    pub fn hash(self: *const @This(), hasher: *std.hash.Fnv1a_64) void {
        var iter = self.variants.iterator();
        while (iter.next()) |entry| {
            const id = entry.key_ptr.*;
            const ty = entry.value_ptr.*;
            hasher.update(id);
            if (ty) |tyy| {
                hasher.update(std.mem.asBytes(&tyy));
            }
        }
    }

    pub fn get_string(self: *const @This(), gpa: std.mem.Allocator, type_map: *types.TypeTbl, source: []const u8) anyerror![]const u8 {
        var out = std.ArrayList(u8).init(gpa);
        var iter = self.variants.iterator();
        try out.appendSlice("enum { ");
        while (iter.next()) |entry| {
            const value = entry.value_ptr.*;
            const key = entry.key_ptr.*;
            try out.appendSlice(key);
            if (value) |val| {
                try out.appendSlice(": ");
                try out.appendSlice(try type_map.get(val).?.get_string(type_map, gpa, source));
                try out.appendSlice(", ");
            }
        }
        try out.appendSlice(" }");
        return try out.toOwnedSlice();
    }

    pub fn equals(self: *const @This(), other: *const @This()) bool {
        if (self.variants.count() != other.variants.count()) {
            return false;
        }

        var iterator = self.variants.iterator();
        while (iterator.next()) |next| {
            if (other.variants.get(next.key_ptr.*)) |val| {
                if (val != next.value_ptr.*) {
                    return false;
                }
            } else {
                return false;
            }
        }
        return true;
    }

};

//a variable decleration ast node
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
    param_types: []TypeId,
    return_ty: TypeId,
    body: ?*Ast,

    pub fn hash(self: *const @This(), type_tbl: *types.TypeTbl, allocator: std.mem.Allocator) !TypeId {
        var args = std.ArrayList(TypeId).init(allocator);
        for (self.param_types) |ty| {
            try args.append(ty);
        }
        const fnctype: Type = .{
            .base_type = .{
                .func = .{
                    .args = try args.toOwnedSlice(),
                    .ret = self.return_ty,
                },
                },
            .modifiers = null,
        };
        const fnctypeid = fnctype.hash();
        _ = try type_tbl.getOrPutValue(fnctypeid, fnctype);
        return fnctypeid;
    }
};


pub const ModuleDecl = struct {
    path: *Ast,
};

pub const Import = struct {
    path: *Ast,
};

pub const ParamList = struct {
    left: *Ast,
    params: []*Ast,
    //add generics here
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
pub const Cast = struct {
    expr: *Ast,
    ty: TypeId
};
pub const Ternary = struct {
    condition: *Ast,
    true_path: *Ast,
    false_path: *Ast
};

pub const Block = struct {
    exprs: []*Ast
};

pub const TypeId = usize;

pub const Ast = struct {
    node: AstNode,
    span: types.Span,
    tyid: ?TypeId,
    scope_id: ?usize = null,
    pub fn create(node: AstNode, span: types.Span) @This() {
        return .{
            .node = node,
            .span = span,
            .tyid = null,
        };
    }
};

//NOTE: these really should all be pointers (atm its about 100 bytes) but since its going to get rewritten I don't really care
pub const AstNode = union(enum) { 
    binary_expr: BinaryExpr, 
    unary_expr: UnaryExpr, 
    terminal: types.Token, 
    type_literal: Type,
    unit,
    assignment: Assignment,  
    if_stmt: IfStmt,
    while_loop: WhileLoop,
    ternary: Ternary,
    block: Block, 
    var_decl: VarDecl, 
    fn_decl: FnDecl, 
    fn_call: ParamList, 
    param_list: ParamList,
    return_stmt: *Ast, 
    type_decl: TypeDecl,
    terminated: *Ast,
    type_cons: TypeCons,
    access_operator: AccessOperator,
    cast: Cast, 
    path: Path,
    module_decl: ModuleDecl,
    import: Import,
    _,
};
