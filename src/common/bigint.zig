const std = @import("std");

const BigInt = struct {
    value: std.math.big.int.Mutable,

    pub fn init(allocator: std.mem.Allocator, value: anytype) !@This() {
        return .{
            .value = .initSet(allocator, value),
        };
    }
};
