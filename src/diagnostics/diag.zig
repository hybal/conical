const std = @import("std");
const common = @import("common");

pub const Severity = enum {
    Note,
    Warning,
    Error,
    Help,
};

pub const LabelStyle = enum {
    primary,
    secondary
};

pub const Label = struct {
    span: common.Span,
    style: LabelStyle,
    message: []const u8
};

pub const Suggestion = struct {
    span: common.Span,
    replacement: []const u8,
    message: []const u8,
    apply: enum {
        always,
        maybe,
        never,
    },
};



pub const Diagnostic = struct {
    code: ErrorCode,
    span: common.Span,
    severity: Severity,
    message: []const u8,
    labels: []const Label,
    notes: []const []const u8,
    help: ?[]const u8,
    suggestions: []const Suggestion,
};


pub const DiagnosticBuilder = struct {
    allocator: std.mem.Allocator,
    _code: ?ErrorCode,
    _span: ?common.Span,
    _severity: ?Severity,
    _message: ?[]const u8,
    _labels: std.ArrayList(Label),
    _notes: std.ArrayList([]const u8),
    _help: ?[]const u8,
    _suggestions: std.ArrayList(Suggestion),

    pub fn init(allocator: std.mem.Allocator) @This() {
        return DiagnosticBuilder {
            .allocator = allocator,
            ._code = null,
            ._severity = null,
            ._message = null,
            ._labels = .empty,
            ._notes = .empty,
            ._help = null,
            ._suggestions = .empty,
            ._span = null,
        };

    }

    pub fn deinit(self: *@This()) void {
        self._labels.deinit();
        self._notes.deinit();
        self._suggestions.deinit();
    }

    const BuildError = error {
        MissingCode,
        MissingSeverity,
        MissingMessage,
        MissingSpan,
    };
    pub fn build(self: *@This()) ( BuildError || std.mem.Allocator.Error )!Diagnostic {
        return Diagnostic {
            .code = self._code orelse return error.MissingCode,
            .severity = self._severity orelse return error.MissingSeverity,
            .message = self._message orelse return error.MissingMessage,
            .span = self._span orelse return error.MissingSpan,
            .labels = try self._labels.toOwnedSlice(self.allocator),
            .help = self._help,
            .notes = try self._notes.toOwnedSlice(self.allocator),
            .suggestions = try self._suggestions.toOwnedSlice(self.allocator),
        };
    }

    pub fn code(self: *@This(), value: ErrorCode) *@This() {
        self._code = value;
        return self;
    }

    pub fn span(self: *@This(), value: common.Span) *@This() {
        self._span = value;
        return self;
    }

    pub fn severity(self: *@This(), value: Severity) *@This() {
        self._severity = value;
        return self;
    }

    pub fn message(self: *@This(), value: []const u8) *@This() {
        self._message = value;
        return self;
    }

    pub fn add_label(self: *@This(), value: Label) !*@This() {
        try self._labels.append(value, self.gpa);
        return self;
    }

    pub fn add_note(self: *@This(), value: []const u8) !*@This() {
        try self._notes.append(value, self.gpa);
        return self;
    }

    pub fn add_suggestion(self: *@This(), value: Suggestion) !*@This() {
        try self._suggestions.append(value, self.gpa);
        return self;
    }

    pub fn help(self: *@This(), value: []const u8) *@This() {
        self._help = value;
        return self;
    }
};

pub const ErrorId = usize;

pub const ErrorStore = struct {
    errors: std.ArrayList(ErrorType),
    gpa: std.mem.Allocator,
    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .errors = .empty,
            .gpa = allocator,
        };
    }
    pub fn deinit(self: *@This()) void {
        self.errors.deinit(self.gpa);
    }
    pub fn push(self: *@This(), err: ErrorType) !ErrorId { 
        try self.errors.append(self.gpa, err);
        return self.errors.items.len;
    }

    pub fn emit(self: *@This(), context: *common.Context, io: std.Io, writer: *std.Io.Writer) !void {
        for (self.errors.items) |err| {
            const diag = try err.to_diagnostic(self.gpa);
            try self.emit_diag(diag, context, io, writer);
        }
    }


    fn emit_diag(self: *@This(), diag: Diagnostic, context: *common.Context, io: std.Io, writer: *std.Io.Writer) !void {
        const file = context.file_store.store.items[diag.span.fileid];
        const buffer = try file.get_source(io, self.gpa);

        const file_path = if (file == .file) 
            file.file.normalize()
            else try std.fmt.allocPrint(self.gpa, "buffer@{}", .{diag.span.fileid});
        const loc = try span_to_line_col(diag.span, buffer);
        const line = get_line(loc.line_end, buffer);
        const line_buf = buffer[line.@"0"..line.@"1"];
        const stripped_line = strip(line_buf);
        const lines = line_buf[stripped_line.@"0"..stripped_line.@"1"];
        const line_start = line.@"0" + stripped_line.@"0";
        const code = diag.code;
        const msg = diag.message;
        try writer.print("[", .{});
        try writer.print("\x1B[0;3;90m{s}\x1B[0m", .{file_path});
        try writer.print(":{}] ", .{loc.line_start});
        try writer.print("\x1B[1;91m(E{x}) error: \x1B[0m", .{ code });
        try writer.print("\x1B[1;37m{s}\x1B[0m\n", .{msg});
        try writer.print("\x1B[32m  |    \x1B[0m{s}\n", .{lines});
        try writer.print("\x1B[32m{s}\x1B[0m\n", .{try get_caret_line(diag.span, line_start, 7 , self.gpa)});
    }


    //For now this only includes line information since the source file is not currently required to be utf-8, 
    // in the future this should also include columns that respect both utf-8 and tabs
    const SpanLoc = struct {
        line_start: usize,
        line_end: usize,
    };
    fn span_to_line_col(span: common.Span, source: []const u8) !SpanLoc {
        var line_start: usize = 1;
        var line_end = line_start;
        // While index < span.start iterate tracking line_start + col_start
        // While index >= span.start && index < span.end track line_end, col_end
        var index: usize = 0;
        while (index < span.end) {
            const c = source[index];
            if (c == '\n') {
                if (index < span.start) line_start += 1;
                if (index > span.start) line_end += 1;
            }
            if (index == span.start) line_end = line_start;
            index += 1;
        }
        return .{ .line_start = line_start, .line_end = line_end };
    }

    fn get_lines(line_start: usize, line_end: usize, source: []const u8) []const u8 {
        const start = get_line(line_start, source);
        const end = get_line(line_end, source);
        return source[start.@"0"..end.@"1"];
    }

    fn get_line(line: usize, source: []const u8) struct {usize, usize }{
        var ln: usize = 1;
        var start: usize = 0;
        var end: usize = 0;
        var index: usize = 0;
        while (index < source.len) {
            const c = source[index];
            if (c == '\n') {
                ln += 1;
            }
            if (ln < line) {
                start += 1;
            }

            if (ln == line) {
                end += 1;
            }

            if (ln > line) break;
            index += 1;
        }
        return .{ start, start + end };
    }

    fn get_caret_line(span: common.Span, line_start: usize, adjustment: usize, allocator: std.mem.Allocator) ![]const u8 {
        const end_adj = span.end - line_start;
        var line: std.ArrayList(u8) = .empty;
        try line.appendNTimes(allocator, ' ', end_adj - 1 + adjustment);
        try line.append(allocator, '^');
        return try line.toOwnedSlice(allocator);
    }
    
};

fn strip(in: []const u8) struct {usize, usize} {
    var start: usize = 0;
    var c = in[start];
    while (std.ascii.isWhitespace(c)) {
        start += 1;
        c = in[start];
    }
    var end = in.len - 1;
    c = in[end];
    while (std.ascii.isWhitespace(c)) {
        end -= 1;
        c = in[end];
    }
    return .{ start, end + 1};
}


pub const ErrorType = struct {
    ptr: *anyopaque,
    vtable: VTable,

    const VTable = struct {
        to_diagnostic: *const fn ( self: *anyopaque, allocator: std.mem.Allocator) anyerror!Diagnostic,
    };

    pub fn to_diagnostic(self: @This(), allocator: std.mem.Allocator) anyerror!Diagnostic { 

        return self.vtable.to_diagnostic(self.ptr, allocator);
    }
};


pub const LEXER_ERROR_RANGE = .{ 0x0000, 0x0010 };
pub const PARSE_ERROR_RANGE = .{ 0x0100, 0x0199 }; 
pub const SYMBOL_RES_ERROR_RANGE = .{ 0x0200, 0x0299 };
pub const TYPE_INFER_ERROR_RANGE = .{ 0x0300, 0x0699 };
pub const GENERAL_SEMA_ERROR_RANGE = .{ 0x0700, 0x0799 };


pub const ErrorCode = u16;
