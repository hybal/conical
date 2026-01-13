const std = @import("std");
comptime {
    _ = @import("lex/mod.zig");
    _ = @import("parse/mod.zig");
    _ = @import("common.zig");
    _ = @import("diagnostics/mod.zig");
    _ = @import("hir/mod.zig");
    _ = @import("mir/mod.zig");
    _ = @import("sema/mod.zig");
    _ = @import("backend/mod.zig");
    _ = @import("bindings/mod.zig");
    _ = @import("driver/mod.zig");
}
