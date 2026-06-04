const std = @import("std");
const hir = @import("hir").hir;
const Hir = hir.Hir;
const common = @import("common");
const types = @import("types");
const tir = @import("./Tir.zig");
const Tir = tir.Tir;


allocator: std.mem.Allocator,
hir_tree: *const Hir,
builder: tir.TirBuilder,
context: *common.Context,
file: common.FileId,

pub fn init(allocator: std.mem.Allocator, hir_tree: *const Hir, context: *common.Context, file: common.FileId, source: []const u8) @This() {
    return .{
        .allocator = allocator,
        .hir_tree = hir_tree,
        .builder = .init(allocator, source),
        .context = context,
        .file = file,
    };
}

