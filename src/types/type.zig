const std = @import("std");
const common = @import("common");
const set = @import("./set.zig");

pub const Type = struct {
    set: set.Set,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) @This() {
        return @This() {
            .value = .{ .sets = .empty },
            .associated = .{ .sets = .empty },
            .allocator = allocator,
        };
    }

};
