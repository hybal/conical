const std = @import("std");
const diag = @import("diagnostics");
const common = @import("common");
const lex = @import("lex");

const ParseErrorKind = enum(u16) {
    unexpected_token = diag.PARSE_ERROR_RANGE.@"0",
    unmatched_delimeter,
    expected_expression,
    expected_block,
    expected_token,
    unexpected_expression,
    unexpected_statement,
    invalid_expression,
    malformed_path,
    malformed_initializer,
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

    pub fn to_diagnostic(self: *@This(), allocator: std.mem.Allocator) !diag.Diagnostic {
        var builder = diag.DiagnosticBuilder.init(allocator);
        try builder
            .code(@intFromEnum(ParseErrorKind.unexpected_token))
            .severity(.Error)
            .span(self.found.span)
            .message("Unexpected token");
        return try builder.build();
    }

    pub fn get_error_type(self: *@This()) diag.ErrorType {
        return diag.ErrorType {
            .ptr = @ptrCast(self),
            .vtable = &self.to_diagnostic,
        };
    }
};

pub const ExpectedExpressionError = struct {
    found: lex.Token,

    pub fn to_diagnostic(self: *@This(), allocator: std.mem.Allocator) !diag.Diagnostic {
        var builder = diag.DiagnosticBuilder.init(allocator);
        try builder
            .code(@intFromEnum(ParseErrorKind.expected_expression))
            .severity(.Error)
            .span(self.found.span)
            .message("Expected expression")
            .add_label(diag.Label {
                .message = "Found this",
                .span = self.found.span,
                .style = .primary
            });
        return try builder.build();
    }

    pub fn get_error_type(self: *@This()) diag.ErrorType {
        return diag.ErrorType {
            .ptr = @ptrCast(self),
            .vtable = &self.to_diagnostic,
        };
    }

};

pub const UnmatchedDelimeterError = struct {
    start_delim: lex.Token,
    expected_delim: lex.Tag,
    span: common.Span,

    pub fn to_diagnostic(self: *@This(), allocator: std.mem.Allocator) !diag.Diagnostic {
        var builder = diag.DiagnosticBuilder.init(allocator);
        try builder
            .code(@intFromEnum(ParseErrorKind.unmatched_delimeter))
            .severity(.Error)
            .span(self.span)
            .message("Unmatched delimeter")
            .add_label(diag.Label {
                .message = "Delimeter started here",
                .span = self.start_delim.span,
                .style = .primary
            });
        return try builder.build();
    }
    pub fn get_error_type(self: *@This()) diag.ErrorType {
        return diag.ErrorType {
            .ptr = @ptrCast(self),
            .vtable = &self.to_diagnostic,
        };
    }

};

pub const MalformedPathError = struct {
    kind: enum {
        trailing,
        consecutive,
        empty,
        single_colon
    },
    span: common.Span,
    malformed_span: common.Span,

    pub fn to_diagnostic(self: *@This(), allocator: std.mem.Allocator) !diag.Diagnostic {
        var builder = diag.DiagnosticBuilder.init(allocator);
        const label_message = switch(self.kind) {
            .trailing => "Trailing '::'",
            .consecutive => "Multiple consecutive '::'",
            .empty => "Single '::'",
            .single_colon => "Expected '::'",
        };

        const note_message = switch(self.kind) {
            .trailing => "A path cannot end with a '::'",
            .consecutive => "Identifiers must appear between '::'",
            .empty => "A path must start with an identifier",
            .single_colon => "A path can only contain '::' and identifiers",
        };
    
        try builder
            .code(@intFromEnum(ParseErrorKind.malformed_path))
            .severity(.Error)
            .span(self.span)
            .message("Malformed path literal")
            .add_label(label_message, self.malformed_span)
            .add_note(note_message);
        return try builder.build();

    }
    pub fn get_error_type(self: *@This()) diag.ErrorType {
        return diag.ErrorType {
            .ptr = @ptrCast(self),
            .vtable = &self.to_diagnostic,
        };
    }

};

pub const MalformedInitializerError = struct {
    kind: enum {
        duplicate_field,
        missing_comma,
        missing_bracket,
    },
    span: common.Span,
    previous_field: ?common.Span,

    pub fn to_diagnostic(self: *@This(), allocator: std.mem.Allocator) !diag.Diagnostic {
        _ = self;
        _ = allocator;
        unreachable;
    }
    pub fn get_error_type(self: *@This()) diag.ErrorType {
        return diag.ErrorType {
            .ptr = @ptrCast(self),
            .vtable = &self.to_diagnostic,
        };
    }

};


