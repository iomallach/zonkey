const std = @import("std");
const tok = @import("token.zig");

const Statement = enum {
    LetStatement,
    ReturnStatement,
    ExpressionStatement,
    BlockStatement,
};

const BlockStatement = struct {
    token: tok.token,
    statements: []*StatementNode,
};
const LetStatement = struct {
    token: tok.Token,
    name: *Identifier,
    value: *ExpressionNode,
};
const ReturnStatement = struct {
    token: tok.Token,
    return_value: *ExpressionNode,
};
const ExpressionStatement = struct {
    token: tok.Token,
    expression: *ExpressionNode,
};

const StatementNode = union(Statement) {
    LetStatement: *LetStatement,
    ReturnStatement: *ReturnStatement,
    ExpressionStatement: *ExpressionStatement,
    BlockStatement: *BlockStatement,
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

const Identifier = struct {
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

const IntegerLiteral = struct {
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

const BooleanLiteral = struct {
    token: tok.Token,
    value: bool,
};

const StringLiteral = struct { token: tok.Token, value: []const u8 };

const ArrayLiteral = struct {
    token: tok.Token,
    elements: []*Expression,
};

const FunctionLiteral = struct {
    token: tok.Token,
    parameters: []*Identifier,
    body: []*BlockStatement,
};

const FunctionCall = struct {
    token: tok.Token,
    function: *Expression,
    arguments: []*Expression,
};

const Prefix = struct {
    token: tok.Token,
    operator: []const u8,
    right: *Expression,
};

const Infix = struct {
    token: tok.Token,
    operator: []const u8,
    left: *Expression,
    right: *Expression,
};

const If = struct {
    token: tok.Token,
    condition: *Expression,
    consequence: []*BlockStatement,
    alternative: []*BlockStatement,
};

const Index = struct {
    token: tok.Token,
    expression: *Expression,
    indexed_expression: *Expression,
};

const ExpressionNode = union(Expression) {
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
};
