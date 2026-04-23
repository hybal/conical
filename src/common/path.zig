const std = @import("std");

pub const Path = struct {
    initial: []const u8,
    normalized: []const u8,

    pub fn create(path: []const u8, allocator: std.mem.Allocator) !@This() {
        return .{
            .initial = path,
            .normalized = try std.fs.path.resolve(allocator, &.{ path })
        };
    }

    pub fn create_relative(root: []const u8, path: []const u8, allocator: std.mem.Allocator) !@This() {
        return .{
            .initial = root,
            .normalized = try std.fs.path.resolve(allocator, &.{root, path}),
        };
    }

    pub fn normalize(self: *const @This()) []const u8 {
        return self.normalized;
    }

    pub fn make_reader(self: *const @This(), io: std.Io, allocator: std.mem.Allocator) !std.Io.Reader {
        const fl = try std.Io.Dir.cwd().openFile(io, self.normalized, .{});
        const reader_buffer = try allocator.alloc(u8, 4096);
        return fl.reader(io, reader_buffer).interface;
    }
};
