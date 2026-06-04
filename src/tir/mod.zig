const _tir = @import("./Tir.zig");
const _eval = @import("./eval.zig");
const _infer = @import("./inference.zig");
const _lower = @import("./lower.zig");

pub const eval = _eval;
pub const Tir = _tir;
pub const infer = _infer;
pub const lower = _lower;
