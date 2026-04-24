const std = @import("std");
const diag = @import("diagnostics");
const common = @import("common");
const lex = @import("lex");
const ast = @import("Ast.zig");
const fmt = std.fmt.allocPrint;

const ParseErrorKind = enum(u16) {
    unexpected_token = diag.PARSE_ERROR_RANGE.@"0",
    expected_declaration,
    unexpected_declaration,
    expected_expression,
    unexpected_expression,
    expected_token,
    malformed_expression,
    malformed_declaration,
    malformed_statement,
    unmatched_delimeter,
    eof,

};

comptime {
    const info = @typeInfo(ParseErrorKind).@"enum";
    for (info.fields) |field| {
        if (field.value > diag.PARSE_ERROR_RANGE.@"1") {
            @compileError("A field in ParseErrorKind exceeds the allowed limit");
        }
    }
}


pub const UnexpectedTokenError = struct {
    found: lex.Token,

    pub fn to_diagnostic(self: *const @This(), allocator: std.mem.Allocator) !diag.Diagnostic {
        var builder = diag.DiagnosticBuilder.init(allocator);
        try builder
            .code(@intFromEnum(ParseErrorKind.unexpected_token))
            .severity(.Error)
            .span(self.found.span)
            .message("Unexpected token");
        return try builder.build();
    }

    pub fn get_error_type(self: *const @This(), allocator: std.mem.Allocator) !diag.ErrorType {
        const vtable = struct {
            pub fn to_diagnostic(_self: *const anyopaque, _allocator: std.mem.Allocator) !diag.Diagnostic {
                return UnexpectedTokenError.to_diagnostic(@alignCast(@ptrCast(_self)), _allocator);
            }
        };

        return diag.ErrorType {

            .ptr = try common.createWith(allocator, self.*),
            .vtable = .{ .to_diagnostic = &vtable.to_diagnostic},
        };
    }
};

pub const ExpectedDeclarationError = struct {
    ty: enum {
        expected_module,
    },
    span: common.Span,

    pub fn to_diagnostic(self: *const @This(), allocator: std.mem.Allocator) !diag.Diagnostic {
        var builder = diag.DiagnosticBuilder.init(allocator);

        const msg = switch (self.ty) {
            .expected_module => "Expected module declaration",
        };

        _ = builder
            .code(@intFromEnum(ParseErrorKind.expected_declaration))
            .severity(.Error)
            .span(self.span)
            .message(msg);
        return try builder.build();
    }

    pub fn get_error_type(self: *const @This(), allocator: std.mem.Allocator) !diag.ErrorType {
        const vtable = struct {
            pub fn to_diagnostic(_self: *const anyopaque, _allocator: std.mem.Allocator) !diag.Diagnostic {
                return ExpectedDeclarationError.to_diagnostic(@alignCast(@ptrCast(_self)), _allocator);
            }
        };

        return diag.ErrorType {

            .ptr = try common.createWith(allocator, self.*),
            .vtable = .{ .to_diagnostic = &vtable.to_diagnostic},
        };
    }
};

pub const UnexpectedDeclarationError = struct {
    span: common.Span,
    ty: ast.AstKind,

    pub fn to_diagnostic(self: *const @This(), allocator: std.mem.Allocator) !diag.Diagnostic {
        var builder = diag.DiagnosticBuilder.init(allocator);
        const msg = try std.fmt.allocPrint(allocator, "Unexpected declaration `{s}`", .{ @tagName(self.ty) });
        try builder
            .code(@intFromEnum(ParseErrorKind.unexpected_declaration))
            .severity(.Error)
            .span(self.span)
            .message(msg);
        return try builder.build();
    }
    pub fn get_error_type(self: *const @This(), allocator: std.mem.Allocator) !diag.ErrorType {
        const vtable = struct {
            pub fn to_diagnostic(_self: *const anyopaque, _allocator: std.mem.Allocator) !diag.Diagnostic {
                return UnexpectedDeclarationError.to_diagnostic(@alignCast(@ptrCast(_self)), _allocator);
            }
        };

        return diag.ErrorType {

            .ptr = try common.createWith(allocator, self.*),
            .vtable = .{ .to_diagnostic = &vtable.to_diagnostic},
        };
    }
};
pub const ExpectedTokenError = struct {
    span: common.Span,
    expected: lex.Tag,

    pub fn to_diagnostic(self: *const @This(), allocator: std.mem.Allocator) !diag.Diagnostic {
        var builder = diag.DiagnosticBuilder.init(allocator);
        const msg = try fmt(allocator, "Expected `{s}`",
            .{
                self.expected.to_string(),
            });
        _ = builder
            .code(@intFromEnum(ParseErrorKind.expected_token))
            .severity(.Error)
            .span(self.span)
            .message(msg);
        return try builder.build();
    }


    pub fn get_error_type(self: *const @This(), allocator: std.mem.Allocator) !diag.ErrorType {
        const vtable = struct {
            pub fn to_diagnostic(_self: *const anyopaque, _allocator: std.mem.Allocator) !diag.Diagnostic {
                return ExpectedTokenError.to_diagnostic(@alignCast(@ptrCast(_self)), _allocator);
            }
        };

        return diag.ErrorType {

            .ptr = try common.createWith(allocator, self.*),
            .vtable = .{ .to_diagnostic = &vtable.to_diagnostic},
        };
    }
};
