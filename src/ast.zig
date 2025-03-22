const std = @import("std");
const tok = @import("token.zig");

const AstNodeKind = enum {
    Program,
    // Statements
    LetStatement,
    ReturnStatement,
    ExpressionStatement,
    BlockStatement,
    // Expressions
    Identifier,
    IntegerLiteral,
    FloatLiteral,
    BooleanLiteral,
    StringLiteral,
    ArrayLiteral,
    FunctionLiteral,
    FunctionCall,
    Prefix,
    Infix,
    If,
    Index,
};

pub const AstNode = union(AstNodeKind) {
    Program: Program,
    LetStatement: LetStatement,
    ReturnStatement: ReturnStatement,
    ExpressionStatement: ExpressionStatement,
    BlockStatement: BlockStatement,
    Identifier: Identifier,
    IntegerLiteral: IntegerLiteral,
    FloatLiteral: FloatLiteral,
    BooleanLiteral: BooleanLiteral,
    StringLiteral: StringLiteral,
    ArrayLiteral: ArrayLiteral,
    FunctionLiteral: FunctionLiteral,
    FunctionCall: FunctionCall,
    Prefix: Prefix,
    Infix: Infix,
    If: If,
    Index: Index,
};

pub const Program = struct {
    program: std.ArrayList(AstNode),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Program {
        return Program{
            .program = std.ArrayList(AstNode).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Program) void {
        self.program.deinit();
    }

    pub fn addStatement(self: *Program, stmt: AstNode) !void {
        try self.program.append(stmt);
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        for (self.program.items) |s| {
            try s.format(fmt, options, writer);
        }
    }
};

pub const LetStatement = struct {
    token: tok.Token,
    name: *AstNode, // Identifier
    value: *AstNode, // ExpressionNode

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        try writer.writeAll(self.token.literal);
        try self.name.format(fmt, options, writer);
        try writer.writeAll(" = ");
        try self.value.format(fmt, options, writer);
        try writer.writeAll(";");
    }
};

pub const ReturnStatement = struct {
    token: tok.Token,
    return_value: *AstNode, // ExpressionNode

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        try writer.writeAll(self.token.literal);
        try self.return_value.format(fmt, options, writer);
        try writer.writeAll(";");
    }
};

pub const ExpressionStatement = struct {
    token: tok.Token,
    expression: *AstNode, // ExpressionNode

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try self.expression.format(fmt, options, writer);
    }
};

pub const BlockStatement = struct {
    token: tok.Token,
    statements: std.ArrayList(AstNode),
    allocator: std.mem.Allocator,

    pub fn init(token: tok.Token, allocator: std.mem.Allocator) BlockStatement {
        return BlockStatement{
            .token = token,
            .statements = std.ArrayList(AstNode).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn addStatement(self: *BlockStatement, stmt: AstNode) !void {
        try self.statements.append(stmt);
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) anyerror!void {
        try writer.writeAll("{\n");
        for (self.statements.items) |s| {
            try s.format(fmt, options, writer);
        }
        try writer.writeAll("\n}");
    }
};

pub const Identifier = struct {
    token: tok.Token,
    value: []const u8,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        if (fmt.len == 0) {
            return writer.writeAll(self.value);
        }
        return self.format("", options, writer);
    }
};

pub const IntegerLiteral = struct {
    token: tok.Token,
    value: i64,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        if (fmt.len == 0) {
            return writer.writeAll(self.token.literal);
        }

        return self.format("", options, writer);
    }
};

pub const FloatLiteral = struct {
    token: tok.Token,
    value: f64,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        _ = fmt;
        _ = options;
        return writer.writeAll(self.token.literal);
    }
};

pub const BooleanLiteral = struct {
    token: tok.Token,
    value: bool,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        if (fmt.len == 0) {
            return writer.writeAll(self.token.literal);
        }

        return self.format("", options, writer);
    }
};

pub const StringLiteral = struct {
    token: tok.Token,
    value: []const u8,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        if (fmt.len == 0) {
            return writer.writeAll(self.value);
        }

        return self.format("", options, writer);
    }
};

pub const ArrayLiteral = struct {
    token: tok.Token,
    elements: std.ArrayList(AstNode),

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        _ = fmt;

        try writer.writeAll("[");

        for (self.elements, 0..) |elem, i| {
            try elem.format("{}", options, writer);

            if (i < self.elements.len - 1) {
                try writer.writeAll(", ");
            }
        }

        try writer.writeAll("]");
    }
};

pub const FunctionLiteral = struct {
    token: tok.Token,
    parameters: std.ArrayList(AstNode),
    body: *AstNode,
    name: ?[]const u8,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll(self.token.literal);
        try writer.writeAll("(");
        for (self.parameters, 0..) |param, i| {
            try param.format(fmt, options, writer);

            if (i < self.parameters.len - 1) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeAll(")");

        for (self.body) |b| {
            try b.format(fmt, options, writer);
        }
    }
};

pub const FunctionCall = struct {
    token: tok.Token,
    function: *AstNode,
    arguments: std.ArrayList(AstNode),

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try self.function.format(fmt, options, writer);
        try writer.writeAll("(");
        for (self.arguments) |a| {
            try a.format(fmt, options, writer);
        }
        try writer.writeAll(")");
    }
};

pub const Prefix = struct {
    token: tok.Token,
    operator: []const u8,
    right: *AstNode,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll("(");
        try writer.writeAll(self.operator);
        try self.right.format(fmt, options, writer);
        try writer.writeAll(")");
    }
};

pub const Infix = struct {
    token: tok.Token,
    operator: []const u8,
    left: *AstNode,
    right: *AstNode,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll("(");
        try self.left.format(fmt, options, writer);
        try writer.writeAll(self.operator);
        try self.right.format(fmt, options, writer);
        try writer.writeAll(")");
    }
};

pub const If = struct {
    token: tok.Token,
    condition: *AstNode,
    consequence: *AstNode,
    alternative: ?*AstNode,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll("if ");
        try self.condition.format(fmt, options, writer);
        try writer.writeAll(" ");
        try self.consequence.format(fmt, options, writer);

        if (self.alternative) |alt| {
            try writer.writeAll("else ");
            for (alt) |stmt| {
                try stmt.format(fmt, options, writer);
            }
        }
    }
};

pub const Index = struct {
    token: tok.Token,
    expression: *AstNode,
    indexed_expression: *AstNode,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll("(");
        try self.expression.format(fmt, options, writer);
        try writer.writeAll("[");
        try self.expression.format(fmt, options, writer);
        try writer.writeAll("])");
    }
};
