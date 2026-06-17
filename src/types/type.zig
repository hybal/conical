const std = @import("std");
const common = @import("common");
const set = @import("./set.zig");


/// Represents a subset of the overal Type set.
/// Two subsets are able to be merged only when label == null
/// Otherwise it is a distinct subset.
pub const Subset = struct {
    set: set.Set,
    label: ?common.intern.InternId,
};

/// Array of products / labeled subsets
pub const TypeSet = struct {
    sets: std.ArrayList(Subset),
};


pub const Type = struct {
    value: TypeSet,
    associated: ?TypeSet,
    allocator: std.mem.Allocator,
    pub fn init(allocator: std.mem.Allocator) @This() {
        return @This() {
            .value = .{ .sets = .empty },
            .associated = .{ .sets = .empty },
            .allocator = allocator,
        };
    }

};
