const std = @import("std");
const types = @import("types.zig");
pub const Severity = enum {
    Error,
    Warning,
    Note,
    pub fn get_color(self: @This()) []const u8 {
        return switch (self) {
            .Error => "\x1b[31m",
            .Warning => "\x1b[33m",
            .Note => "\x1b[32m"
        };
    }
};

pub const Diag = struct {
    severity: Severity,
    message: []const u8,
    span: types.Span
};



pub const Session = struct {
    diags: std.ArrayList(Diag),
    gpa: std.mem.Allocator,
    source: []const u8,
    pub fn init(allocator: std.mem.Allocator, source: []const u8) @This() {
        return .{
            .diags = .init(allocator),
            .source = source,
            .gpa = allocator
        };
    }

    pub fn deinit(self: *@This()) void {
        self.diags.deinit();
    }

    pub fn emit(self: *@This(), severity: Severity, span: types.Span, message: []const u8) !void {
        try self.diags.append(.{
            .severity = severity,
            .message = message,
            .span = span
        });
    }

    pub fn flush(self: *@This(), writer: anytype) !void {
        for (self.diags.items) |diag| {
            const line, const col = self.get_loc(diag);
            const src_line = std.mem.trim(u8, self.get_line(diag), " \t\r\n");
            const new_start = @intFromPtr(src_line.ptr) - @intFromPtr(self.source.ptr);
            const start = diag.span.end - new_start;
            try writer.print(
                "{s}{s} [{};{}]:\x1b[0m {s}\n\t{s}\n\t{s}\n",
                .{
                    diag.severity.get_color(),
                    @tagName(diag.severity),
                    line,
                    col,
                    diag.message,
                    src_line,
                    try self.get_marker_line(start)
                });
                    
        }
    }
    
    fn is_eol(char: u8) bool {
        return switch (char) {
            0x0...0x1F, 0x7F => true,
            else => false
        };
    }

    fn get_marker_line(self: *@This(), cursor: usize) ![]const u8 {
        var out = std.ArrayList(u8).init(self.gpa);
        try out.appendNTimes(' ', cursor);
        try out.append('^');
        return out.toOwnedSlice();
    }
    fn get_line(self: *@This(), diag: Diag) []const u8 {
        var start = diag.span.start;
        var end = diag.span.end;
        //if (start == end and start != 0) {
        //    start -= 1;
        //}
        var found_start = false;
        var found_end = false;
        while (true) {
            if (!found_start) {
                if (is_eol(self.source[start]) or start == 0) {
                    found_start = true;
                } else {
                    start -= 1;
                }
            }
            if (!found_end) {
                if (is_eol(self.source[end]) or end == self.source.len) {
                    found_end = true;
                } else {
                    end += 1;
                }
            }
            if (found_start and found_end) break;
        }
        return self.source[start..end];
    }

    fn get_loc(self: *@This(), diag: Diag) struct { usize, usize } {
        var line: usize = 1;
        var col: usize = 1;
        for (self.source, 0..) |char, i| {
            if (i == diag.span.start) break; 
            switch (char) {
                '\n' => {
                    line += 1;
                    col = 0;
                },
                '\r' => {
                    if (i + 1 < self.source.len and self.source[i + 1] == '\n') continue;
                    line += 1;
                    col = 0;
                },
                else => col += 1
            }
        }
        return .{ line, col};

    }

};


