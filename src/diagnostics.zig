const std = @import("std");
const Token = @import("token.zig").Token;

const red = "\x1b[31m";
const reset = "\x1b[0m";

pub const let_statement_error_fmt =
    \\ {s} expected expression to be of type '{any}', defined type was '{any}', but got '{any}' instead at line {d}, column {d}
    \\ {s}
    \\ {s}^ here
    \\
;
pub const return_statement_error_fmt =
    \\ {s} the type of the return statement does not match the declared return function type: expected '{any}', got '{any}' at line {d}, column {d}
    \\ {s}
    \\ {s}^ here
    \\
;
pub const assignment_statement_error_fmt =
    \\ {s} identifier {s} is of type '{any}', attempting to assign a value of type '{any}' at line {d}, column {d}
    \\ {s}
    \\ {s}^ here
    \\
;
pub const function_call_error_fmt =
    \\ {s} function {s} expects {d} arguments, but {d} were given at line {d}, column {d}
    \\ {s}
    \\ {s}^ here
    \\
;
pub const function_call_param_error_fmt =
    \\ {s} parameter {d} of function {s} expects type '{any}', but got '{any}' at line {d}, column {d}
    \\ {s}
    \\ {s}^ here
    \\
;
pub const if_expression_cond_error_fmt =
    \\ {s} if condition must be a boolean expression, got '{any}' instead at line {d}, columnd {d}
    \\ {s}
    \\ {s}^ here
    \\
;
pub const if_expression_type_error_fmt =
    \\ {s} types of an if expression are ambiguous, got '{any}' at then, '{any}' at else at line {d}, column {d}
    \\ {s}
    \\ {s}^ here
    \\
;
pub const prefix_expression_type_error_fmt =
    \\ {s} incompatible type '{any}', expected '{s}' at line {d}, column {d}
    \\ {s}
    \\ {s}^ here
    \\
;
pub const infix_expression_type_eror_fmt =
    \\ {s} incompatible types '{any}' and '{any}' at line {d}, column {d}
    \\ {s}
    \\ {s}^ here
    \\
;
pub const unbound_identifier_error_fmt =
    \\ {s} unbound identifier '{s}' at line {d}, column {d}
    \\ {s}
    \\ {s}^ here
    \\
;
pub const lexer_error_fmt =
    \\ expected next char to be {c}, got {c} instead at line {d}, column {d}
    \\ {s}
    \\ {s} ^here
    \\
;
pub const parser_peek_error_fmt =
    \\ Expected next token to be {any}, got {any} instead at line {d} column {d}
    \\ {s}
    \\ {s}^ here
    \\
;
pub const parser_parselet_error_fmt =
    \\ Unexpected token {any} encountered at line {d} column {d}
    \\ {s}
    \\ {s}^ here
    \\
;

pub const Diagnostics = struct {
    errors: std.ArrayList([]const u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Diagnostics {
        return Diagnostics{
            .errors = std.ArrayList([]const u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn reportError(self: *Diagnostics, comptime fmt: []const u8, args: anytype) error{OutOfMemory}!void {
        try self.errors.append(try std.fmt.allocPrint(self.allocator, fmt, args));
    }

    pub fn getErrorMarker(self: *Diagnostics) error{OutOfMemory}![]const u8 {
        return try std.fmt.allocPrint(self.allocator, "{s}error:{s}", .{ red, reset });
    }

    pub fn getErrorPointerSpaces(self: *Diagnostics, token: *const Token) error{OutOfMemory}![]const u8 {
        const spaces = try self.allocator.alloc(u8, token.span.start);
        @memset(spaces, ' ');
        return spaces;
    }

    pub fn showAndFail(self: *Diagnostics) error{CompilationFailed}!void {
        if (self.errors.items.len > 0) {
            for (self.errors.items) |e| {
                std.debug.print("{s}\n", .{e});
            }
            return error.CompilationFailed;
        }
    }
};
