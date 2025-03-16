const std = @import("std");
const tok = @import("token.zig");

const Ast = enum {
    Statement,
    Expression,
    Program,
};

pub const AstNode = union(Ast) { StatementNode: *StatementNode, ExpressionNode: *ExpressionNode, ProgramNode: *Program };

pub const Program = struct {
    program: std.ArrayList(StatementNode),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Program {
        return Program{
            .program = std.ArrayList(StatementNode).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Program) void {
        self.program.deinit();
    }

    pub fn addStatement(self: *Program, stmt: StatementNode) !void {
        try self.program.append(stmt);
    }
};

pub const Statement = enum {
    LetStatement,
    ReturnStatement,
    ExpressionStatement,
    BlockStatement,
};

pub const BlockStatement = struct {
    token: tok.Token,
    statements: std.ArrayList(StatementNode),
    allocator: std.mem.Allocator,

    pub fn init(token: tok.Token, allocator: std.mem.Allocator) BlockStatement {
        return BlockStatement{
            .token = token,
            .statements = std.ArrayList(StatementNode).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn addStatement(self: *BlockStatement, stmt: StatementNode) !void {
        try self.statements.append(stmt);
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll("{\n");
        for (self.statements) |s| {
            s.format(fmt, options, writer);
        }
        try writer.writeAll("\n}");
    }
};

pub const LetStatement = struct {
    token: tok.Token,
    name: Identifier,
    value: ExpressionNode,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll("{} ", self.token.literal);
        try self.name.format(fmt, options, writer);
        try writer.writeAll(" = ");
        try self.value.format(fmt, options, writer);
        try writer.writerAll(";");
    }
};

pub const ReturnStatement = struct {
    token: tok.Token,
    return_value: ExpressionNode,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll("{} ", .{self.token.literal});
        try self.return_value.format(fmt, options, writer);
        try writer.writerAll(";");
    }
};

pub const ExpressionStatement = struct {
    token: tok.Token,
    expression: ExpressionNode,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try self.expression.format(fmt, options, writer);
    }
};

pub const StatementNode = union(Statement) {
    LetStatement: LetStatement,
    ReturnStatement: ReturnStatement,
    ExpressionStatement: ExpressionStatement,
    BlockStatement: BlockStatement,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .LetStatement => |let_stmt| try let_stmt.format(fmt, options, writer),
            .ReturnStatement => |ret_stmt| try ret_stmt.format(fmt, options, writer),
            .ExpressionStatement => |expr_stmt| try expr_stmt.format(fmt, options, writer),
            .BlockStatement => |block_stmt| try block_stmt.format(fmt, options, writer),
        }
    }

    pub fn createLetStatement(identifier: Identifier, token: tok.Token, expression: ExpressionNode) StatementNode {
        return StatementNode{ .LetStatement = LetStatement{
            .name = identifier,
            .token = token,
            .value = expression,
        } };
    }
};

const Expression = enum {
    Identifier,
    IntegerLiteral,
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

pub const Identifier = struct {
    token: tok.Token,
    value: []const u8,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (fmt.len == 0) {
            return writer.print("{s}", .{self.value});
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
        writer: anytype,
    ) !void {
        if (fmt.len == 0) {
            return writer.print("{s}", .{self.token.literal});
        }

        return self.format("", options, writer);
    }
};

pub const BooleanLiteral = struct {
    token: tok.Token,
    value: bool,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (fmt.len == 0) {
            return writer.print("{s}", .{self.token.literal});
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
        writer: anytype,
    ) !void {
        if (fmt.len == 0) {
            return writer.print("{s}", .{self.value});
        }

        return self.format("", options, writer);
    }
};

pub const ArrayLiteral = struct {
    token: tok.Token,
    elements: []*ExpressionNode,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
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
    parameters: []*Identifier,
    body: []*BlockStatement,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll("{s}", .{self.token.literal});
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
    function: *ExpressionNode,
    arguments: []*ExpressionNode,

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
    right: *ExpressionNode,

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
    left: *ExpressionNode,
    right: *ExpressionNode,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll("(");
        try self.left.format(fmt, options, writer);
        try writer.writeAll(" {s} ", self.operator);
        try self.right.format(fmt, options, writer);
        try writer.writeAll(")");
    }
};

pub const If = struct {
    token: tok.Token,
    condition: *ExpressionNode,
    consequence: BlockStatement,
    alternative: ?BlockStatement,

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
    expression: *ExpressionNode,
    indexed_expression: *ExpressionNode,

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

pub const ExpressionNode = union(Expression) {
    Identifier: Identifier,
    IntegerLiteral: IntegerLiteral,
    BooleanLiteral: BooleanLiteral,
    StringLiteral: StringLiteral,
    ArrayLiteral: ArrayLiteral,
    FunctionLiteral: FunctionLiteral,
    FunctionCall: FunctionCall,
    Prefix: Prefix,
    Infix: Infix,
    If: If,
    Index: Index,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .Identifier => |ident| try ident.format(fmt, options, writer),
            .StringLiteral => |str| try str.format(fmt, options, writer),
            else => {
                @panic("unimplemented");
            },
        }
    }
};

const test_helpers = struct {
    pub fn make_dummy_token_span() tok.TokenSpan {
        return tok.TokenSpan{
            .start = 0,
            .end = 0,
            .line_number = 0,
            .source_chunk = "",
        };
    }
    pub fn make_token(literal: []const u8, tt: tok.TokenType) tok.Token {
        return tok.Token{
            .literal = literal,
            .token_type = tt,
            .span = make_dummy_token_span(),
        };
    }
    pub fn make_string_literal(value: []const u8) StringLiteral {
        return StringLiteral{ .token = make_token(value, tok.TokenType.STRING), .value = value };
    }
};

test "Test string literal format" {
    const str_literal = test_helpers.make_string_literal("test");

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const deinit_status = gpa.deinit();
        std.debug.assert(deinit_status == .ok);
    }

    const lit_str = try std.fmt.allocPrint(allocator, "{}", .{str_literal});
    defer allocator.free(lit_str);

    try std.testing.expectEqualStrings("test", lit_str);
}

test "Test array literal format" {
    var elem1 = ExpressionNode{ .StringLiteral = test_helpers.make_string_literal("test1") };
    var elem2 = ExpressionNode{ .StringLiteral = test_helpers.make_string_literal("test2") };
    var elems = [_]*ExpressionNode{ &elem1, &elem2 };
    const str_literal = ArrayLiteral{ .elements = &elems, .token = test_helpers.make_token("", tok.TokenType.LBRACKET) };

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const deinit_status = gpa.deinit();
        std.debug.assert(deinit_status == .ok);
    }

    const lit_str = try std.fmt.allocPrint(allocator, "{}", .{str_literal});
    defer allocator.free(lit_str);

    try std.testing.expectEqualStrings("[test1, test2]", lit_str);
}
