const std = @import("std");
const Hir = @import("Hir.zig");
const Ast = @import("Ast.zig");
const types = @import("types.zig");
const diag = @import("diag.zig");
const llvm = @import("llvm");

allocator: std.mem.Allocator,
