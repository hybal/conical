const std = @import("std");
const common = @import("common");
const types = @import("types");
const eval = @import("./eval.zig");
const tir = @import("./Tir.zig");
const Tir = tir.Tir;

allocator: std.mem.Allocator,
typemap: tir.TypeMap,
builder: tir.TirBuilder,
interpreter: eval.Interpreter,
current_block: tir.BlockId,
