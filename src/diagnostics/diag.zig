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
            ._labels = .init(allocator),
            ._notes = .init(allocator),
            ._help = null,
            ._suggestions = .init(allocator),
        };
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
            .labels = try self._labels.toOwnedSlice(),
            .help = self._help,
            .notes = try self._notes.toOwnedSlice(),
            .suggestions = try self._suggestions.toOwnedSlice(),
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
        try self._labels.append(value);
        return self;
    }

    pub fn add_note(self: *@This(), value: []const u8) !*@This() {
        try self._notes.append(value);
        return self;
    }

    pub fn add_suggestion(self: *@This(), value: Suggestion) !*@This() {
        try self._suggestions.append(value);
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

    pub fn init(allocator: std.mem.Allocator) !@This() {
        return .{
            .errors = .init(allocator),
        };
    }
    pub fn push(self: *@This(), err: ErrorType) !ErrorId { 
        try self.errors.append(err);
        return self.errors.items.len;
    }
};

pub const ErrorType = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    const VTable = struct {
        to_diagnostic: fn ( self: *anyopaque, allocator: std.mem.Allocator) anyerror!Diagnostic,
    };

    pub fn to_diagnostic(self: @This(), allocator: std.mem.Allocator) anyerror!Diagnostic { 

        return self.vtable.to_diagnostic(self.ptr, allocator);
    }
};


pub const LEXER_ERROR_RANGE = .{ 0x0000, 0x0010 };
pub const PARSE_ERROR_RANGE = .{ 0x0100, 0x0199 }; 
pub const SYMBOL_RES_ERROR_RANGE = .{ 0x0200, 0x0299 };
pub const TYPE_INFER_ERROR_RANGE = .{ 0x0300, 0x699 };
pub const GENERAL_SEMA_ERROR_RANGE = .{ 0x0700, 0x0799 };


pub const ErrorCode = enum(u16) {
    
};
