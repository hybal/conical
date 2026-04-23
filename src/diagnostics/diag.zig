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
        var reader = if (file == .buffer)
            std.Io.Reader.fixed(file.buffer)
            else
                try file.file.make_reader(io, self.gpa);

        const file_path = if (file == .file) 
            file.file.normalize()
            else "buffer";
        const loc = try span_to_line_col(diag.span, &reader);
        const code = diag.code;
        const msg = diag.message;
        try common.color.Color.RED.print(writer);
        try writer.print("Error(E{x})[{s}:{}] {s}\n", .{ code, file_path, loc.line_start, msg });
        try common.color.Color.RESET.print(writer);
    }


    //For now this only includes line information since the source file is not currently required to be utf-8, 
    // in the future this should also include columns that respect both utf-8 and tabs
    const SpanLoc = struct {
        line_start: usize,
        line_end: usize,
    };
    fn span_to_line_col(span: common.Span, reader: *std.Io.Reader) !SpanLoc {
        var line_start: usize = 1;
        var line_end = line_start;
        // While index < span.start iterate tracking line_start + col_start
        // While index >= span.start && index < span.end track line_end, col_end
        var index: usize = 0;
        while (index < span.end) {
            const c = reader.takeByte() catch |e| if (e == std.Io.Reader.Error.EndOfStream) null else return e;
            if (c == null) {
                break;
            }
            if (c.? == '\n') {
                if (index < span.start) line_start += 1;
                if (index == span.start) line_end = line_start;
                if (index > span.start) line_end += 1;
            }
            index += 1;
        }
        return .{ .line_start = line_start, .line_end = line_end };
    }

    
};


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
