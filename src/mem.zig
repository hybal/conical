const std = @import("std");

pub inline fn createWith(allocator: std.mem.Allocator, val: anytype) std.mem.Allocator.Error!*@TypeOf(val) {
    const allocation = try allocator.create(@TypeOf(val));
    allocation.* = val;
    return allocation;
}
