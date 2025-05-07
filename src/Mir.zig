const std = @import("std");
const ast = @import("Ast.zig");

const Module = struct {
    functions: []Function,
    globals: []Global,
    types: []TypeDecl,
};


const Function = struct {
    id: []const u8,
    params: Parameter,
    return_ty: TypeId,
    blocks: []Block,
};

