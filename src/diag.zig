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
            const src_span = self.get_line(diag);
            const src_line = self.source[src_span.start..src_span.end];
            try writer.print(
                "{s}{s} [{};{}]: {s}\x1b[0m\n{s}\n{s}{s}\x1b[0m\n",
                .{
                    diag.severity.get_color(),
                    @tagName(diag.severity),
                    line,
                    col,
                    diag.message,
                    src_line,
                    diag.severity.get_color(),
                    try self.get_marker_line(src_span.length, diag.span)
                });
                    
        }
    }
    
    fn get_marker_line(self: *@This(), cursor: usize, span: types.Span) ![] const u8 {
        var out = std.ArrayList(u8).init(self.gpa);
        if (cursor > 0) {
            try out.appendNTimes(' ', cursor - 1);
        }
        for (0..(span.end - span.start)) |_|{
            try out.append('^');
        }
        return out.toOwnedSlice();
    }
    fn get_line(self: *@This(), diag: Diag) struct {length: usize, start: usize, end: usize}  {
        var start = diag.span.start;
        var end = diag.span.end;
        var length: usize = 0;
        var found_start = false;
        var found_end = false;
        while (!found_start or !found_end) {
            if (!found_start) {
                if (start == 0) {
                    found_start = true;
                    length += 1;
                } else {
                    switch (self.source[start]) {
                        '\n', '\r', 0 => found_start = true,
                        '\t' => {
                            length += 4;
                            start -= 1;
                        },
                        else => {
                            start -= 1;
                            length += 1;
                        }
                    }
                }
            }
            if (!found_end) {
                switch(self.source[end]) {
                    '\n', '\r', 0 => found_end = true,
                    else => end += 1
                }
            }
        }
        return .{.length = length, .start = start, .end = end};
    }

    fn get_loc(self: *@This(), diag: Diag) struct { usize, usize } {
        var line: usize = 1;
        var col: usize = 0;
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
                else => |val| switch(val) {
                    '\t' => col += 7,
                    else => col += 1
                }
            }
        }
        return .{ line, col};

    }

};


