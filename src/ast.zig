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

const StatementNode = union(Statement) {
    LetStatement: struct {
        token: tok.Token,
        name: *Identifier,
        value: *ExpressionNode,
    },
    ReturnStatement: struct {
        token: tok.Token,
        return_value: *ExpressionNode,
    },
    ExpressionStatement: struct {
        token: tok.Token,
        expression: *ExpressionNode,
    },
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
};

const ExpressionNode = union(Expression) {
    Identifier: Identifier,
    IntegerLiteral: struct {
        token: tok.Token,
        value: i64,
    },
    BooleanLiteral: struct {
        token: tok.Token,
        value: bool,
    },
    StringLiteral: struct { token: tok.Token, value: []const u8 },
    ArrayLiteral: struct {
        token: tok.Token,
        elements: []*ExpressionNode,
    },
    FunctionLiteral: struct {
        token: tok.Token,
        parameters: []*Identifier,
        body: []*BlockStatement,
    },
    FunctionCall: struct {
        token: tok.Token,
        function: *ExpressionNode,
        arguments: []*ExpressionNode,
    },
    Prefix: struct {
        token: tok.Token,
        operator: []const u8,
        right: *ExpressionNode,
    },
    Infix: struct {
        token: tok.Token,
        operator: []const u8,
        left: *ExpressionNode,
        right: *ExpressionNode,
    },
    If: struct {
        token: tok.Token,
        condition: *ExpressionNode,
        consequence: []*BlockStatement,
        alternative: []*BlockStatement,
    },
    Index: struct {
        token: tok.Token,
        expression: *ExpressionNode,
        indexed_expression: *ExpressionNode,
    },
};
