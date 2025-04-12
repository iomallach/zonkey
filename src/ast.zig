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
    Void,
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

        if (std.mem.eql(u8, "void", literal)) {
            return .Void;
        }
        unreachable;
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
    Program: Program,
    LetStatement: LetStatement,
    ReturnStatement: ReturnStatement,
    ExpressionStatement: ExpressionStatement,
    BlockStatement: BlockStatement,
    WhileLoopStatement: WhileLoopStatement,
    AssignmentStatement: AssignmentStatement,
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
};

pub const LetStatement = struct {
    token: tok.Token,
    name: *AstNode, // Identifier
    expression: *AstNode, // ExpressionNode
    inferred_type: ?Type,
};

pub const ReturnStatement = struct {
    token: tok.Token,
    return_value: *AstNode, // ExpressionNode
};

pub const ExpressionStatement = struct {
    token: tok.Token,
    expression: *AstNode, // ExpressionNode
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
};

pub const WhileLoopStatement = struct {
    token: tok.Token,
    condition: *AstNode, // Expression
    statements: *AstNode, // BlockStatement
};

pub const AssignmentStatement = struct {
    token: tok.Token,
    name: *AstNode,
    expression: *AstNode,
};

pub const Identifier = struct {
    token: tok.Token,
    value: []const u8,
};

pub const IntegerLiteral = struct {
    token: tok.Token,
    value: i64,
};

pub const FloatLiteral = struct {
    token: tok.Token,
    value: f64,
};

pub const BooleanLiteral = struct {
    token: tok.Token,
    value: bool,
};

pub const StringLiteral = struct {
    token: tok.Token,
    value: []const u8,
};

pub const ArrayLiteral = struct {
    token: tok.Token,
    elements: std.ArrayList(AstNode),
};

pub const FunctionLiteral = struct {
    token: tok.Token,
    parameters: std.ArrayList(AstNode),
    body: *AstNode,
    name: ?[]const u8,
    return_type: Type,
};

pub const FunctionParameter = struct {
    ident: *AstNode, // Identifier,
    inferred_type: Type,
};

pub const FunctionCall = struct {
    token: tok.Token,
    function: *AstNode,
    arguments: std.ArrayList(AstNode),
};

pub const BuiltInCall = struct {
    token: tok.Token,
    function: *AstNode,
    argument: *AstNode,
};

pub const Prefix = struct {
    token: tok.Token,
    operator: UnaryOp,
    right: *AstNode,
};

pub const Infix = struct {
    token: tok.Token,
    operator: BinaryOp,
    left: *AstNode,
    right: *AstNode,
};

pub const If = struct {
    token: tok.Token,
    condition: *AstNode,
    consequence: *AstNode,
    alternative: ?*AstNode,
};

pub const Index = struct {
    token: tok.Token,
    expression: *AstNode,
    indexed_expression: *AstNode,
};
