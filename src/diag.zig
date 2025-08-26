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
            std.debug.print("DEBUG: span: {any}, sorce: {s}\n", .{diag.span, diag.span.get_string(self.source)});
            const src_line = self.get_line(diag.span);
            const line, const col = self.get_loc(diag.span);
            const caret_line = try self.get_caret_line(src_line, diag.span);
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
                    caret_line,
                });
                    
        }
    }
    //FIXME: for some reason does not work with spans that are the full line
    fn get_caret_line(self: *@This(), line: []const u8, span: types.Span) ![]const u8 {
        var out = std.ArrayList(u8).init(std.heap.page_allocator);
        const line_start = @intFromPtr(line.ptr) - @intFromPtr(self.source.ptr);
        const span_start = line_start - span.start;
        const span_len = span.end - span.start;
        for (0..span_start) |_| {
            try out.append('-');
        }
        for (0..span_len) |_| {
            try out.append('^');
        }
        return try out.toOwnedSlice();
    }

    fn get_line(self: *@This(), span: types.Span) []const u8 {
        var start = span.start;
        var end = span.end;
        var start_c = self.source[start];
        var end_c = self.source[end];
        var at_start = false;
        var at_end = false;
        while (!at_start or !at_end) {
            if (start_c == '\n' or start == 0) {
                at_start = true;
            }
            if (end_c == '\r' or end_c == '\n' or end_c == 0 or end >= self.source.len) {
                at_end = true;
            }
            if (!at_start) {
                start_c = self.source[start];
                start -= 1;
            }
            if (!at_end) {
                end_c = self.source[end];
                end += 1;
            }
                   }
        const stripped = strip(self.source[start + 1..end ]);
        return stripped;
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
        while (i < span.start) {
            const c = self.source[i];
            i += 1;
            switch (c) {
                '\n', '\r' => {
                    line += 1;
                    col = 1;
                },
                '\t' => {
                    col += 8;
                },
                else => {
                    col += 1;
                }
            }
        }
        return .{ line, col };
    }
    
    
};


