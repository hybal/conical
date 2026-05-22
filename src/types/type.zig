const std = @import("std");
const common = @import("common");
const set = @import("./set.zig");

pub const TypeSet = struct {
    set: set.Set,
    label: ?common.intern.InternId,
};

pub const TypeId = usize;

pub const TypeStore = []Type;

pub const Type = struct {
    value: []set.Set,
    associated: set.Set,
    min_bits: usize,
    size: ?usize,

};
