
const std = @import("std");

pub const InternId = u64;


pub const InternPool = struct {
    pool: std.AutoHashMap(InternId, []const u8),

    const SEED = 0;

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .pool = .init(allocator),
        };
    }

    pub fn put(self: *@This(), string: []const u8) !InternId {
        const id = std.hash.XxHash3.hash(SEED, string);
        try self.pool.getOrPutValue(id, string);
        return id;
    }

    pub fn get(self: *@This(), id: InternId) ?[]const u8 {
        return self.pool.get(id);
    }

    pub fn getId(self: *@This(), string: []const u8) InternId {
        _ = self;
        return std.hash.XxHash3.hash(SEED, string);
    }

    pub fn has_string(self: *@This(), string: []const u8) bool {
        const id = std.hash.XxHash3.hash(SEED, string);
        return self.pool.contains(id);
    }
};

