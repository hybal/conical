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
    frozen: bool,
    pub fn init(allocator: std.mem.Allocator, source: []const u8) @This() {
        return .{
            .diags = .init(allocator),
            .source = source,
            .gpa = allocator,
            .frozen = false
        };
    }
    pub fn freeze(self: *@This()) void {
        self.frozen = true;
    }

    pub fn unfreeze(self: *@This()) void {
        self.frozen = false;
    }
    pub fn deinit(self: *@This()) void {
        self.diags.deinit();
    }

    pub fn emit(self: *@This(), severity: Severity, span: types.Span, message: []const u8) !void {
        if (self.frozen) return;
        try self.diags.append(.{
            .severity = severity,
            .message = message,
            .span = span
        });
    }

    //FIXME: this is completely broken
    pub fn flush(self: *@This(), writer: anytype) !void {
        for (self.diags.items) |diag| {
            //const src_line = self.get_line(diag.span);
            const line, const col = self.get_loc(diag.span);
            //const caret_line, const print_line = try self.get_caret_line(src_line, diag.span);
            try writer.print(
                "{s}{s} [{};{}]: {s}\x1b[0m\n    \n    \x1b[0m\n",
                .{
                    diag.severity.get_color(),
                    @tagName(diag.severity),
                    line,
                    col,
                    diag.message,
                    //print_line,
                    //diag.severity.get_color(),
                    //caret_line,
                });
                    
        }
    }
    //FIXME: for some reason does not work with spans that are the full line
    fn get_caret_line(self: *@This(), line: []const u8, in_span: types.Span) !struct {[]const u8, []const u8} {
        var out = std.ArrayList(u8).init(self.gpa);
        const adjusted_span_start, const compact_line = try self.compact(in_span.start, line);
        const line_start = @intFromPtr(line.ptr) - @intFromPtr(self.source.ptr);
        const span_start = adjusted_span_start - line_start;
        const span_len = in_span.end - in_span.start;
        for (0..span_start) |_| {
            try out.append(' ');
        }
        for (0..span_len) |_| {
            try out.append('^');
        }
        return .{ try out.toOwnedSlice(), compact_line };
    }

    fn get_line(self: *@This(), span: types.Span) []const u8 {
        const start = @max(0, @min(span.start, self.source.len));
        const end = @max(0, @min(span.end, self.source.len));

        var new_start = start;
        while (new_start > 0 and self.source[new_start - 1] != '\n') {
            new_start -= 1;
        }
        if (new_start > 0 and self.source[new_start - 1] == '\n') {
            new_start += 1;
        }

        var new_end = end;
        while (new_end < self.source.len and self.source[new_end] != '\n') {
            new_end += 1;
        }
        if (new_end < self.source.len and self.source[new_end] == '\n') {
            new_end += 1;
        }
        return self.source[new_start..new_end];
    }

    fn compact(self: *@This(), span_start: usize, string: []const u8) !struct {usize, []const u8} {
        var out: std.ArrayList(u8) = try .initCapacity(self.gpa, string.len);
        var shift_left: usize = 0;
        var prev_space = false;
        for (string) |c| {
            switch (c) {
                ' ', '\t' => {
                    if (!prev_space) {
                        prev_space = true;
                        try out.append(' ');
                    } else {
                        shift_left += 1;
                    }
                },
                '\n', '\r' => {},
                else => {
                    if (prev_space) {
                        prev_space = false;
                    }
                    try out.append(c);
                }
            }
        }
        return .{
            span_start - shift_left,
            try out.toOwnedSlice()
        };
    }

    fn strip(string: []const u8) []const u8 {
        var i: usize = 0;
        var j = string.len - 1;
        var c = string[i];
        while (i < string.len and std.ascii.isWhitespace(c)) {
            i += 1;
            c = string[i];
        }
        c = string[j];
        while (j > 0 and std.ascii.isWhitespace(c)) {
            j -= 1;
            c = string[j];
        }
        return string[i..j - 1];
    }

    fn get_loc(self: *@This(), span: types.Span) struct { usize, usize } {
        var line: usize = 1;
        var col: usize = 1;
        var i: usize = 0;
        while (i != span.start) {
            const c = self.source[i];
            i += 1;
            switch (c) {
                '\n', '\r' => {
                    line += 1;
                    col = 1;
                },
                '\t' => {
                    col += 9;
                },
                else => {
                    col += 1;
                }
            }
        }
        return .{ line, col};
    }
    
    
};


