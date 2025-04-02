const std = @import("std");
const tok = @import("token.zig");

pub const UnaryOp = enum {
    Negation,
    Minus,

    pub fn fromString(str: []const u8) UnaryOp {
        if (std.mem.eql(u8, str, "-")) {
            return UnaryOp.Minus;
        } else if (std.mem.eql(u8, str, "!")) {
            return UnaryOp.Negation;
        }
        unreachable;
    }

    pub fn toString(self: UnaryOp) []const u8 {
        return switch (self) {
            .Negation => "!",
            .Minus => "-",
        };
    }
};

pub const BinaryOp = enum {
    Plus,
    Minus,
    EqualEqual,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Multiply,
    Divide,

    pub fn fromString(str: []const u8) BinaryOp {
        if (std.mem.eql(u8, str, "+")) {
            return BinaryOp.Plus;
        } else if (std.mem.eql(u8, str, "-")) {
            return BinaryOp.Minus;
        } else if (std.mem.eql(u8, str, "==")) {
            return BinaryOp.EqualEqual;
        } else if (std.mem.eql(u8, str, "!=")) {
            return BinaryOp.NotEqual;
        } else if (std.mem.eql(u8, str, ">")) {
            return BinaryOp.Greater;
        } else if (std.mem.eql(u8, str, "<")) {
            return BinaryOp.Less;
        } else if (std.mem.eql(u8, str, ">=")) {
            return BinaryOp.GreaterEqual;
        } else if (std.mem.eql(u8, str, "<=")) {
            return BinaryOp.LessEqual;
        } else if (std.mem.eql(u8, str, "*")) {
            return BinaryOp.Multiply;
        } else if (std.mem.eql(u8, str, "/")) {
            return BinaryOp.Divide;
        }
        unreachable;
    }

    pub fn toString(self: BinaryOp) []const u8 {
        return switch (self) {
            .Plus => "+",
            .Minus => "-",
            .EqualEqual => "==",
            .NotEqual => "!=",
            .Greater => ">",
            .Less => "<",
            .GreaterEqual => ">=",
            .LessEqual => "<=",
            .Multiply => "*",
            .Divide => "/",
        };
    }
};

pub const PrimitiveType = enum {
    Bool,
    Integer,
    Float,
    String,
    Void,
    Function,

    pub fn fromLiteral(literal: []const u8) PrimitiveType {
        if (std.mem.eql(u8, literal, "bool")) {
            return PrimitiveType.Bool;
        }

        if (std.mem.eql(u8, "int", literal)) {
            return PrimitiveType.Integer;
        }

        if (std.mem.eql(u8, "float", literal)) {
            return PrimitiveType.Float;
        }

        if (std.mem.eql(u8, "string", literal)) {
            return PrimitiveType.String;
        }

        if (std.mem.eql(u8, "void", literal)) {
            return PrimitiveType.Void;
        }
        unreachable;
    }
};

pub const Type = union(enum) {
    PrimitiveType: PrimitiveType,
    Function: *FunctionType,
};

pub const FunctionType = struct {
    return_type: Type,
    arg_types: std.ArrayList(Type),

    pub fn init(func_params: *std.ArrayList(AstNode), ret_type: Type, allocator: std.mem.Allocator) !FunctionType {
        var types = std.ArrayList(Type).init(allocator);
        for (func_params.*.items) |fp| {
            try types.append(fp.FunctionParameter.inferred_type);
        }
        return FunctionType{
            .return_type = ret_type,
            .arg_types = types,
        };
    }
};

const AstNodeKind = enum {
    Program,
    // Statements
    LetStatement,
    ReturnStatement,
    ExpressionStatement,
    BlockStatement,
    WhileLoopStatement,
    // Expressions
    Identifier,
    IntegerLiteral,
    FloatLiteral,
    BooleanLiteral,
    StringLiteral,
    ArrayLiteral,
    FunctionLiteral,
    FunctionParameter,
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
    WhileLoopStatement: WhileLoopStatement,
    Identifier: Identifier,
    IntegerLiteral: IntegerLiteral,
    FloatLiteral: FloatLiteral,
    BooleanLiteral: BooleanLiteral,
    StringLiteral: StringLiteral,
    ArrayLiteral: ArrayLiteral,
    FunctionLiteral: FunctionLiteral,
    FunctionParameter: FunctionParameter,
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
    inferred_type: ?Type,

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

pub const WhileLoopStatement = struct {
    token: tok.Token,
    condition: *AstNode, // Expression
    statements: *AstNode, // BlockStatement
};

pub const Identifier = struct {
    token: tok.Token,
    value: []const u8,
    inferred_type: ?Type,

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
    inferred_type: ?Type,

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
    inferred_type: ?Type,

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
    inferred_type: ?Type,

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
    inferred_type: ?Type,

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
    return_type: Type,

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

pub const FunctionParameter = struct {
    ident: *AstNode, // Identifier,
    inferred_type: Type,
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
    operator: UnaryOp,
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
    operator: BinaryOp,
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
