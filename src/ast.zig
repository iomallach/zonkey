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

pub const Type = union(enum) {
    Bool,
    Integer,
    Float,
    String,
    Unit,
    Function: *FunctionType,

    pub fn primitiveFromLiteral(literal: []const u8) Type {
        if (std.mem.eql(u8, literal, "bool")) {
            return .Bool;
        }

        if (std.mem.eql(u8, "int", literal)) {
            return .Integer;
        }

        if (std.mem.eql(u8, "float", literal)) {
            return .Float;
        }

        if (std.mem.eql(u8, "string", literal)) {
            return .String;
        }

        if (std.mem.eql(u8, "()", literal)) {
            return .Unit;
        }
        unreachable;
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Bool => try writer.writeAll("bool"),
            .Integer => try writer.writeAll("int"),
            .Float => try writer.writeAll("float"),
            .String => try writer.writeAll("string"),
            .Unit => try writer.writeAll("void"),
            .Function => try writer.writeAll("function"),
        }
    }
};

pub const FunctionType = struct {
    return_type: Type,
    arg_types: std.ArrayList(Type),

    pub fn init(func_params: *const std.ArrayList(AstNode), ret_type: Type, allocator: std.mem.Allocator) !FunctionType {
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
    AssignmentStatement,
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
    BuiltInCall,
    Prefix,
    Infix,
    If,
    Index,
};

pub const AstNode = union(AstNodeKind) {
    // Statements
    Program: Program,
    LetStatement: LetStatement,
    ReturnStatement: ReturnStatement,
    ExpressionStatement: ExpressionStatement,
    BlockStatement: BlockStatement,
    WhileLoopStatement: WhileLoopStatement,
    AssignmentStatement: AssignmentStatement,
    // Expressions
    Identifier: Identifier,
    IntegerLiteral: IntegerLiteral,
    FloatLiteral: FloatLiteral,
    BooleanLiteral: BooleanLiteral,
    StringLiteral: StringLiteral,
    ArrayLiteral: ArrayLiteral,
    FunctionLiteral: FunctionLiteral,
    FunctionParameter: FunctionParameter,
    FunctionCall: FunctionCall,
    BuiltInCall: BuiltInCall,
    Prefix: Prefix,
    Infix: Infix,
    If: If,
    Index: Index,

    pub fn getToken(self: *const AstNode) *const tok.Token {
        return switch (self.*) {
            .Program => |n| n.getToken(),
            .LetStatement => |n| n.getToken(),
            .ReturnStatement => |n| n.getToken(),
            .ExpressionStatement => |n| n.getToken(),
            .BlockStatement => |n| n.getToken(),
            .WhileLoopStatement => |n| n.getToken(),
            .AssignmentStatement => |n| n.getToken(),
            .Identifier => |n| n.getToken(),
            .IntegerLiteral => |n| n.getToken(),
            .FloatLiteral => |n| n.getToken(),
            .BooleanLiteral => |n| n.getToken(),
            .StringLiteral => |n| n.getToken(),
            .ArrayLiteral => |n| n.getToken(),
            .FunctionLiteral => |n| n.getToken(),
            .FunctionParameter => |n| n.getToken(),
            .FunctionCall => |n| n.getToken(),
            .BuiltInCall => |n| n.getToken(),
            .Prefix => |n| n.getToken(),
            .Infix => |n| n.getToken(),
            .If => |n| n.getToken(),
            .Index => |n| n.getToken(),
        };
    }
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

    pub fn getToken(self: *const Program) *const tok.Token {
        if (self.program.items.len > 0) {
            return self.program.items[0].getToken();
        }
        @panic("Calling getToken on an empty program");
    }
};

pub const LetStatement = struct {
    token: tok.Token,
    name: *AstNode, // Identifier
    expression: *AstNode, // ExpressionNode
    inferred_type: ?Type,

    pub fn getToken(self: *const LetStatement) *const tok.Token {
        return &self.token;
    }
};

pub const ReturnStatement = struct {
    token: tok.Token,
    return_value: *AstNode, // ExpressionNode

    pub fn getToken(self: *const ReturnStatement) *const tok.Token {
        return &self.token;
    }
};

pub const ExpressionStatement = struct {
    token: tok.Token,
    expression: *AstNode, // ExpressionNode
    discarded: bool,

    pub fn getToken(self: *const ExpressionStatement) *const tok.Token {
        return &self.token;
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

    pub fn getToken(self: *const BlockStatement) *const tok.Token {
        return &self.token;
    }
};

pub const WhileLoopStatement = struct {
    token: tok.Token,
    condition: *AstNode, // Expression
    statements: *AstNode, // BlockStatement

    pub fn getToken(self: *const WhileLoopStatement) *const tok.Token {
        return &self.token;
    }
};

pub const AssignmentStatement = struct {
    token: tok.Token,
    name: *AstNode,
    expression: *AstNode,

    pub fn getToken(self: *const AssignmentStatement) *const tok.Token {
        return &self.token;
    }
};

pub const Identifier = struct {
    token: tok.Token,
    value: []const u8,

    pub fn getToken(self: *const Identifier) *const tok.Token {
        return &self.token;
    }
};

pub const IntegerLiteral = struct {
    token: tok.Token,
    value: i64,

    pub fn getToken(self: *const IntegerLiteral) *const tok.Token {
        return &self.token;
    }
};

pub const FloatLiteral = struct {
    token: tok.Token,
    value: f64,

    pub fn getToken(self: *const FloatLiteral) *const tok.Token {
        return &self.token;
    }
};

pub const BooleanLiteral = struct {
    token: tok.Token,
    value: bool,

    pub fn getToken(self: *const BooleanLiteral) *const tok.Token {
        return &self.token;
    }
};

pub const StringLiteral = struct {
    token: tok.Token,
    value: []const u8,

    pub fn getToken(self: *const StringLiteral) *const tok.Token {
        return &self.token;
    }
};

pub const ArrayLiteral = struct {
    token: tok.Token,
    elements: std.ArrayList(AstNode),

    pub fn getToken(self: *const ArrayLiteral) *const tok.Token {
        return &self.token;
    }
};

pub const FunctionLiteral = struct {
    token: tok.Token,
    parameters: std.ArrayList(AstNode),
    body: *AstNode,
    name: ?[]const u8,
    return_type: Type,

    pub fn getToken(self: *const FunctionLiteral) *const tok.Token {
        return &self.token;
    }
};

pub const FunctionParameter = struct {
    token: tok.Token,
    ident: *AstNode, // Identifier,
    inferred_type: Type,

    pub fn getToken(self: *const FunctionParameter) *const tok.Token {
        return &self.token;
    }
};

pub const FunctionCall = struct {
    token: tok.Token,
    function: *AstNode,
    arguments: std.ArrayList(AstNode),

    pub fn getToken(self: *const FunctionCall) *const tok.Token {
        return &self.token;
    }
};

pub const BuiltInCall = struct {
    token: tok.Token,
    function: *AstNode,
    argument: *AstNode,

    pub fn getToken(self: *const BuiltInCall) *const tok.Token {
        return &self.token;
    }
};

pub const Prefix = struct {
    token: tok.Token,
    operator: UnaryOp,
    right: *AstNode,

    pub fn getToken(self: *const Prefix) *const tok.Token {
        return &self.token;
    }
};

pub const Infix = struct {
    token: tok.Token,
    operator: BinaryOp,
    left: *AstNode,
    right: *AstNode,

    pub fn getToken(self: *const Infix) *const tok.Token {
        return &self.token;
    }
};

pub const If = struct {
    token: tok.Token,
    condition: *AstNode,
    consequence: *AstNode,
    alternative: ?*AstNode,

    pub fn getToken(self: *const If) *const tok.Token {
        return &self.token;
    }
};

pub const Index = struct {
    token: tok.Token,
    expression: *AstNode,
    indexed_expression: *AstNode,

    pub fn getToken(self: *const Index) *const tok.Token {
        return &self.token;
    }
};
