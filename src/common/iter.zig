const std = @import("std");

pub const UnicodeError = error {
    InvalidUtf8,
};
pub const Iterator = struct {
    iter: std.unicode.Utf8Iterator,

    pub fn init(text: []const u8) UnicodeError!@This() {
        const iter = std.unicode.Utf8View.init(text) catch return UnicodeError.InvalidUtf8;
        return .{
            .iter = iter.iterator(),
        };
    }

    pub fn has_next(self: *@This()) bool {
        if (self.iter.peek(1).len == 0) return false;
        return true;
    }

    pub fn next(self: *@This()) ?u21 {
        return self.iter.nextCodepoint();
    }

    pub fn ncLength(self: *@This()) u8 {
        const peekc = std.unicode.utf8CodepointSequenceLength(self.peek() orelse 0) catch unreachable;
        return peekc;
    }

    pub fn peek(self: *@This()) ?u21 {
        const saved_i = self.iter.i;
        const out = self.iter.nextCodepoint();
        self.iter.i = saved_i;
        return out;
    }

    pub fn restore(self: *@This(), index: usize) void {
        self.iter.i = index;
    }

};
