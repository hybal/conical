
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
    severity: Severity,
    message: []const u8,
    labels: []const Label,
    notes: []const []const u8,
    help: ?[]const u8,
    suggestions: []const Suggestion,
};

pub const ErrorStore = struct {
    errors: std.ArrayList(ErrorType),

    pub fn init(allocator: std.mem.Allocator) !@This() {
        return .{
            .errors = .init(allocator),
        };
    }
    pub fn push(self: *@This(), err: ErrorType) !void { 
        try self.errors.append(err);
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


pub const ErrorCode = enum(u16) {
    _,
};
