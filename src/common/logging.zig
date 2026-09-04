const std = @import("std");

pub const Options = struct {

};

pub const Logger = struct {
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer.Allocating,
    options: Options,

    pub fn log(self: *@This(), comptime fmt: []const u8, args: anytype) !void {
        try self.writer.writer.print(fmt, args);
        try self.writer.writer.flush();
        const info = try std.debug.getSelfDebugInfo();
    }
};
