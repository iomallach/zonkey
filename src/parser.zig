const std = @import("std");
const ast = @import("ast.zig");
const tok = @import("token.zig");

const prefixParserFn = fn (*Parser) error{ OutOfMemory, UnexpectedToken }!ast.AstNode;
const infixParserFn = fn (*Parser, ast.AstNode) error{ OutOfMemory, UnexpectedToken }!ast.AstNode;

const Precedence = enum(u8) {
    LOWEST = 1,
    EQUALS, // ==
    LESSGREATER, // <, >, >=, <=
    SUM, // +
    PRODUCT, // *
    PREFIX, // -X, !X
    CALL, // fun(a, b)
    INDEX, // array[index]
};

fn precedence(tt: tok.TokenType) Precedence {
    return switch (tt) {
        .EQUAL_EQUAL => Precedence.EQUALS,
        .BANG_EQUAL => Precedence.EQUALS,
        .LESS => Precedence.LESSGREATER,
        .LESS_EQUAL => Precedence.LESSGREATER,
        .GREATER => Precedence.LESSGREATER,
        .GREATER_EQUAL => Precedence.LESSGREATER,
        .PLUS => Precedence.SUM,
        .MINUS => Precedence.SUM,
        .SLASH => Precedence.PRODUCT,
        .ASTERISK => Precedence.PRODUCT,
        .LPAREN => Precedence.CALL,
        .LBRACKET => Precedence.INDEX,
        else => Precedence.LOWEST,
    };
}

pub const Parser = struct {
    tokens: []tok.Token,
    position: usize,
    errors_list: std.ArrayList([]const u8),

    alloc: std.mem.Allocator,

    prefix_fns: std.AutoHashMap(tok.TokenType, *const prefixParserFn),
    infix_fns: std.AutoHashMap(tok.TokenType, *const infixParserFn),

    pub fn init(tokens: []tok.Token, alloc: std.mem.Allocator) !Parser {
        const errors_list = std.ArrayList([]const u8).init(alloc);
        const prefix_fns = std.AutoHashMap(tok.TokenType, *const prefixParserFn).init(alloc);
        const infix_fns = std.AutoHashMap(tok.TokenType, *const infixParserFn).init(alloc);

        var parser = Parser{ .tokens = tokens, .position = 0, .errors_list = errors_list, .prefix_fns = prefix_fns, .infix_fns = infix_fns, .alloc = alloc };
        try parser.registerParsers();

        return parser;
    }

    fn registerParsers(self: *Parser) !void {
        try self.prefix_fns.put(tok.TokenType.INT, Parser.parseIntegerLiteral);
        try self.prefix_fns.put(tok.TokenType.FLOAT, Parser.parseFloatLiteral);
        try self.prefix_fns.put(tok.TokenType.IDENT, Parser.parseIdentifier);
        try self.prefix_fns.put(tok.TokenType.STRING, Parser.parseStringLiteral);
        try self.prefix_fns.put(tok.TokenType.BANG, Parser.parsePrefixExpression);
        try self.prefix_fns.put(tok.TokenType.MINUS, Parser.parsePrefixExpression);
        try self.prefix_fns.put(tok.TokenType.TRUE, Parser.parseBoolean);
        try self.prefix_fns.put(tok.TokenType.FALSE, Parser.parseBoolean);
        try self.prefix_fns.put(tok.TokenType.LPAREN, Parser.parseGroupedExpression);
        try self.prefix_fns.put(tok.TokenType.IF, Parser.parseIfExpression);
        try self.prefix_fns.put(tok.TokenType.FUNCTION, Parser.parseFunctionLiteral);
        try self.prefix_fns.put(tok.TokenType.LBRACKET, Parser.parseArrayLiteral);

        try self.infix_fns.put(tok.TokenType.PLUS, Parser.parseInfixExpression);
        try self.infix_fns.put(tok.TokenType.MINUS, Parser.parseInfixExpression);
        try self.infix_fns.put(tok.TokenType.ASTERISK, Parser.parseInfixExpression);
        try self.infix_fns.put(tok.TokenType.SLASH, Parser.parseInfixExpression);
        try self.infix_fns.put(tok.TokenType.GREATER, Parser.parseInfixExpression);
        try self.infix_fns.put(tok.TokenType.GREATER_EQUAL, Parser.parseInfixExpression);
        try self.infix_fns.put(tok.TokenType.LESS, Parser.parseInfixExpression);
        try self.infix_fns.put(tok.TokenType.LESS_EQUAL, Parser.parseInfixExpression);
        try self.infix_fns.put(tok.TokenType.EQUAL_EQUAL, Parser.parseInfixExpression);
        try self.infix_fns.put(tok.TokenType.BANG_EQUAL, Parser.parseInfixExpression);
        try self.infix_fns.put(tok.TokenType.LPAREN, Parser.parseFunctionCall);
        try self.infix_fns.put(tok.TokenType.LBRACKET, Parser.parseIndexExpression);
    }

    pub fn deinit(self: *Parser) void {
        self.errors_list.deinit();
        self.prefix_fns.deinit();
        self.infix_fns.deinit();
    }

    fn hasNext(self: *Parser) bool {
        return (self.position + 1 < self.tokens.len);
    }

    fn peekToken(self: *Parser) tok.Token {
        if (self.hasNext()) {
            return self.tokens[self.position + 1];
        }
        return self.tokens[self.position];
    }

    fn advance(self: *Parser) void {
        if (self.hasNext()) {
            self.position += 1;
        }
    }

    fn currentToken(self: *Parser) tok.Token {
        return self.tokens[self.position];
    }

    fn currentPrecedence(self: *Parser) Precedence {
        return precedence(self.currentToken().token_type);
    }

    fn peekPrecedence(self: *Parser) Precedence {
        return precedence(self.peekToken().token_type);
    }

    fn matchPeekTokenType(self: *Parser, tt: tok.TokenType) bool {
        return self.peekToken().token_type == tt;
    }

    fn matchCurrentTokenType(self: *Parser, tt: tok.TokenType) bool {
        return self.currentToken().token_type == tt;
    }

    fn matchNextAndAdvance(self: *Parser, tt: tok.TokenType) error{ OutOfMemory, UnexpectedToken }!void {
        if (!self.matchPeekTokenType(tt)) {
            try self.peekError(tt);
            return error.UnexpectedToken;
        }
        self.advance();
    }

    fn peekError(self: *Parser, tt: tok.TokenType) error{OutOfMemory}!void {
        const token = self.peekToken();
        const spaces = try self.alloc.alloc(u8, token.span.start);
        @memset(spaces, ' ');

        const error_message = try std.fmt.allocPrint(self.alloc,
            \\ Expected next token to be {any}, got {any} instead at line {d} column {d}
            \\ {s}
            \\ {s}^ here
            \\
        , .{
            tt,
            token.token_type,
            token.span.line_number,
            token.span.start,
            token.span.source_chunk,
            spaces,
        });
        try self.errors_list.append(error_message);
    }

    fn parseletError(self: *Parser) error{OutOfMemory}!void {
        const token = self.currentToken();
        const spaces = try self.alloc.alloc(u8, token.span.start);
        @memset(spaces, ' ');

        const error_message = try std.fmt.allocPrint(self.alloc,
            \\ Unexpected token {any} encountered at line {d} column {d}
            \\ {s}
            \\ {s}^ here
            \\
        , .{
            token.token_type,
            token.span.line_number,
            token.span.start,
            token.span.source_chunk,
            spaces,
        });
        try self.errors_list.append(error_message);
    }

    pub fn errors(self: *Parser) *std.ArrayList([]const u8) {
        return &self.errors_list;
    }

    pub fn parse(self: *Parser) error{OutOfMemory}!ast.AstNode {
        var program = ast.Program.init(self.alloc);

        while (!self.matchCurrentTokenType(tok.TokenType.EOF)) {
            const stmt = try self.parseStatement();

            if (stmt) |s| {
                try program.addStatement(s);
            }
            self.advance();
        }

        return ast.AstNode{
            .Program = program,
        };
    }

    fn parseStatement(self: *Parser) error{OutOfMemory}!?ast.AstNode {
        const node = switch (self.currentToken().token_type) {
            tok.TokenType.LET => self.parseLetStatement(),
            tok.TokenType.FUNCTION => self.parseFunctionDeclaration(),
            tok.TokenType.RETURN => self.parseReturnStatement(),
            tok.TokenType.WHILE => self.parseWhileLoopStatement(),
            tok.TokenType.IDENT => blk: {
                if (self.matchPeekTokenType(tok.TokenType.EQUAL)) {
                    break :blk self.parseAssignmentStatement();
                } else {
                    break :blk self.parseExpressionStatement();
                }
            },
            else => self.parseExpressionStatement(),
        };
        return node catch |err| {
            switch (err) {
                error.OutOfMemory => |oom| return oom,
                //TODO: skip to the next statement, e.g. next ;
                error.UnexpectedToken => return null,
            }
        };
    }

    fn parseLetStatement(self: *Parser) error{ OutOfMemory, UnexpectedToken }!ast.AstNode {
        // already know current token is a let token
        const let_token = self.currentToken();
        // expect next to be an identifier
        try self.matchNextAndAdvance(tok.TokenType.IDENT);
        const ident_token = self.currentToken();

        //if there is :, parse the type
        var type_annotation: ?ast.Type = null;
        if (self.matchPeekTokenType(tok.TokenType.COLON)) {
            self.advance();
            try self.matchNextAndAdvance(tok.TokenType.Type);
            //FIXME: what if there is no valid type annotation?
            //FIXME: what if there is a composite aka function type?
            //TODO: implement function type parsing
            type_annotation = self.parsePrimitiveTypeAnnotation();
        }

        //expect = sign
        try self.matchNextAndAdvance(tok.TokenType.EQUAL);
        self.advance();

        const expression = try self.parseExpression(Precedence.LOWEST);
        const heap_expression = try self.alloc.create(ast.AstNode);
        heap_expression.* = expression;

        if (self.matchPeekTokenType(tok.TokenType.SEMICOLON)) {
            self.advance();
        }

        const heap_identifier = try self.alloc.create(ast.AstNode);
        heap_identifier.* = ast.AstNode{
            .Identifier = ast.Identifier{
                .token = ident_token,
                .value = ident_token.literal,
            },
        };

        return ast.AstNode{
            .LetStatement = ast.LetStatement{
                .token = let_token,
                .name = heap_identifier,
                .expression = heap_expression,
                .inferred_type = type_annotation,
            },
        };
    }

    fn parseReturnStatement(self: *Parser) error{ OutOfMemory, UnexpectedToken }!ast.AstNode {
        const return_token = self.currentToken();
        self.advance();

        const return_expression = try self.parseExpression(Precedence.LOWEST);
        const heap_return_expression = try self.alloc.create(ast.AstNode);
        heap_return_expression.* = return_expression;

        if (self.matchPeekTokenType(tok.TokenType.SEMICOLON)) {
            self.advance();
        }

        return ast.AstNode{ .ReturnStatement = ast.ReturnStatement{
            .token = return_token,
            .return_value = heap_return_expression,
        } };
    }

    fn parseExpressionStatement(self: *Parser) error{ OutOfMemory, UnexpectedToken }!ast.AstNode {
        const token = self.currentToken();
        const expression = try self.parseExpression(Precedence.LOWEST);
        const heap_expression = try self.alloc.create(ast.AstNode);
        heap_expression.* = expression;

        if (self.matchPeekTokenType(tok.TokenType.SEMICOLON)) {
            self.advance();
        }

        return ast.AstNode{
            .ExpressionStatement = ast.ExpressionStatement{
                .token = token,
                .expression = heap_expression,
            },
        };
    }

    fn parseExpression(self: *Parser, prec: Precedence) error{ OutOfMemory, UnexpectedToken }!ast.AstNode {
        const prefix_parselet = self.prefix_fns.get(self.currentToken().token_type) orelse {
            try self.parseletError();
            return error.UnexpectedToken;
        };
        var left_expr = try prefix_parselet(self);

        while (!self.matchPeekTokenType(tok.TokenType.SEMICOLON) and @intFromEnum(prec) < @intFromEnum(self.peekPrecedence())) {
            const infix_parselet = self.infix_fns.get(self.peekToken().token_type) orelse {
                // FIXME: this has to be for the peek token (meaning parseletError reports current token)
                try self.parseletError();
                return error.UnexpectedToken;
            };
            self.advance();
            left_expr = try infix_parselet(self, left_expr);
        }

        return left_expr;
    }

    fn parseIntegerLiteral(self: *Parser) !ast.AstNode {
        const token = self.currentToken();
        return ast.AstNode{
            .IntegerLiteral = ast.IntegerLiteral{
                .token = token,
                .value = std.fmt.parseInt(i64, token.literal, 10) catch {
                    unreachable;
                },
            },
        };
    }

    fn parseFloatLiteral(self: *Parser) !ast.AstNode {
        const token = self.currentToken();
        return ast.AstNode{
            .FloatLiteral = ast.FloatLiteral{
                .token = token,
                .value = std.fmt.parseFloat(f64, token.literal) catch {
                    unreachable;
                },
            },
        };
    }

    fn parseIdentifier(self: *Parser) !ast.AstNode {
        const token = self.currentToken();
        return ast.AstNode{ .Identifier = ast.Identifier{
            .token = token,
            .value = token.literal,
        } };
    }

    fn parseStringLiteral(self: *Parser) !ast.AstNode {
        const token = self.currentToken();
        return ast.AstNode{
            .StringLiteral = ast.StringLiteral{
                .token = token,
                .value = token.literal,
            },
        };
    }

    fn parsePrefixExpression(self: *Parser) error{ OutOfMemory, UnexpectedToken }!ast.AstNode {
        // current token is e.g. - or !
        const token = self.currentToken();
        // move the pointer to the expression
        self.advance();
        // parse the expression
        const expression = try self.parseExpression(Precedence.PREFIX);

        // We have to allocate it on the heap due to recursive structure: ExpressionNode -> Prefix -> ExpressionNode
        const heapExpressionNode = try self.alloc.create(ast.AstNode);
        heapExpressionNode.* = expression;

        return ast.AstNode{ .Prefix = ast.Prefix{
            .operator = ast.UnaryOp.fromString(token.literal),
            .token = token,
            .right = heapExpressionNode,
        } };
    }

    fn parseBoolean(self: *Parser) !ast.AstNode {
        const token = self.currentToken();
        return ast.AstNode{
            .BooleanLiteral = ast.BooleanLiteral{
                .token = token,
                .value = self.matchCurrentTokenType(tok.TokenType.TRUE),
            },
        };
    }

    fn parseGroupedExpression(self: *Parser) error{ OutOfMemory, UnexpectedToken }!ast.AstNode {
        // skip to the next token
        self.advance();

        const grouped_expression = try self.parseExpression(Precedence.LOWEST);

        // grouped expression must end with a matching closing ")"
        try self.matchNextAndAdvance(tok.TokenType.RPAREN);

        return grouped_expression;
    }

    fn parseInfixExpression(self: *Parser, left: ast.AstNode) error{ OutOfMemory, UnexpectedToken }!ast.AstNode {
        // the operator of the infix expression, e.g. +, -, *, /, etc.
        const token = self.currentToken();
        const cur_precedence = self.currentPrecedence();
        // skip to the token starting the next expression
        self.advance();
        // parse right expression
        const expression = try self.parseExpression(cur_precedence);

        // have to allocate on the heap due to recursive structure
        const heapRightExpressionNode = try self.alloc.create(ast.AstNode);
        heapRightExpressionNode.* = expression;
        const heapLeftExpressionNode = try self.alloc.create(ast.AstNode);
        heapLeftExpressionNode.* = left;

        return ast.AstNode{ .Infix = ast.Infix{
            .token = token,
            .operator = ast.BinaryOp.fromString(token.literal),
            .left = heapLeftExpressionNode,
            .right = heapRightExpressionNode,
        } };
    }

    fn parseIfExpression(self: *Parser) error{ OutOfMemory, UnexpectedToken }!ast.AstNode {
        const if_token = self.currentToken();
        // advance to the start of the expression
        self.advance();
        const if_expression = try self.parseExpression(Precedence.LOWEST);
        const heap_if_expression = try self.alloc.create(ast.AstNode);
        heap_if_expression.* = if_expression;

        try self.matchNextAndAdvance(tok.TokenType.LBRACE);

        const consequence = try self.parseBlockStatements();
        const heap_consequence = try self.alloc.create(ast.AstNode);
        heap_consequence.* = consequence;

        if (self.matchPeekTokenType(tok.TokenType.ELSE)) {
            self.advance();

            try self.matchNextAndAdvance(tok.TokenType.LBRACE);
            const alternative = try self.parseBlockStatements();
            const heap_alternative = try self.alloc.create(ast.AstNode);
            heap_alternative.* = alternative;

            return ast.AstNode{
                .If = ast.If{
                    .token = if_token,
                    .condition = heap_if_expression,
                    .consequence = heap_consequence,
                    .alternative = heap_alternative,
                },
            };
        }

        return ast.AstNode{ .If = ast.If{
            .token = if_token,
            .condition = heap_if_expression,
            .consequence = heap_consequence,
            .alternative = null,
        } };
    }

    fn parseBlockStatements(self: *Parser) error{ OutOfMemory, UnexpectedToken }!ast.AstNode {
        const lbrace_token = self.currentToken();
        self.advance();

        var block_statements = ast.BlockStatement.init(lbrace_token, self.alloc);

        while (!self.matchCurrentTokenType(tok.TokenType.RBRACE) and !self.matchCurrentTokenType(tok.TokenType.EOF)) {
            // FIXME: should record an error and skip to the next statement
            //TODO: take a second look at how this should be handled
            const statement = try self.parseStatement();
            if (statement) |s| {
                try block_statements.addStatement(s);
            }
            // we're looking at the last token after each parselet, so skipping it
            self.advance();
        }

        return ast.AstNode{
            .BlockStatement = block_statements,
        };
    }

    fn parseFunctionDeclaration(self: *Parser) error{ OutOfMemory, UnexpectedToken }!ast.AstNode {
        const fn_token = self.currentToken();

        try self.matchNextAndAdvance(tok.TokenType.IDENT);
        const fn_ident = self.currentToken();

        try self.matchNextAndAdvance(tok.TokenType.LPAREN);

        const parameters = try self.parseFunctionParameters();

        try self.matchNextAndAdvance(tok.TokenType.Type);
        const return_type = self.parsePrimitiveTypeAnnotation();

        try self.matchNextAndAdvance(tok.TokenType.LBRACE);

        const fn_body_statements = try self.parseBlockStatements();
        const heap_fn_body_statements = try self.alloc.create(ast.AstNode);
        heap_fn_body_statements.* = fn_body_statements;

        if (self.matchPeekTokenType(tok.TokenType.SEMICOLON)) {
            self.advance();
        }

        return ast.AstNode{ .FunctionLiteral = ast.FunctionLiteral{
            .token = fn_token,
            .parameters = parameters,
            .body = heap_fn_body_statements,
            .name = fn_ident.literal,
            .return_type = return_type,
        } };
    }

    fn parseFunctionLiteral(self: *Parser) error{ OutOfMemory, UnexpectedToken }!ast.AstNode {
        const fn_token = self.currentToken();

        try self.matchNextAndAdvance(tok.TokenType.LPAREN);

        const parameters = try self.parseFunctionParameters();

        try self.matchNextAndAdvance(tok.TokenType.Type);
        const return_type = self.parsePrimitiveTypeAnnotation();

        try self.matchNextAndAdvance(tok.TokenType.LBRACE);

        const fn_body_statements = try self.parseBlockStatements();
        const heap_fn_body_statements = try self.alloc.create(ast.AstNode);
        heap_fn_body_statements.* = fn_body_statements;

        return ast.AstNode{ .FunctionLiteral = ast.FunctionLiteral{
            .token = fn_token,
            .parameters = parameters,
            .body = heap_fn_body_statements,
            .name = null,
            .return_type = return_type,
        } };
    }

    fn parseFunctionParameters(self: *Parser) error{ OutOfMemory, UnexpectedToken }!std.ArrayList(ast.AstNode) {
        var parameters = std.ArrayList(ast.AstNode).init(self.alloc);
        if (self.matchPeekTokenType(tok.TokenType.RPAREN)) {
            self.advance();
            return parameters;
        }

        try self.matchNextAndAdvance(tok.TokenType.IDENT);

        var ident = ast.Identifier{
            .token = self.currentToken(),
            .value = self.currentToken().literal,
        };
        var heap_ident = try self.alloc.create(ast.AstNode);
        heap_ident.* = ast.AstNode{
            .Identifier = ident,
        };
        //FIXME: duplicated in a few places now
        try self.matchNextAndAdvance(tok.TokenType.COLON);
        try self.matchNextAndAdvance(tok.TokenType.Type);

        var type_annotation = self.parsePrimitiveTypeAnnotation();
        try parameters.append(ast.AstNode{ .FunctionParameter = ast.FunctionParameter{
            .ident = heap_ident,
            .inferred_type = type_annotation,
        } });

        while (self.matchPeekTokenType(tok.TokenType.COMMA)) {
            self.advance();
            try self.matchNextAndAdvance(tok.TokenType.IDENT);

            ident = ast.Identifier{
                .token = self.currentToken(),
                .value = self.currentToken().literal,
            };
            heap_ident = try self.alloc.create(ast.AstNode);
            heap_ident.* = ast.AstNode{
                .Identifier = ident,
            };
            try self.matchNextAndAdvance(tok.TokenType.COLON);
            try self.matchNextAndAdvance(tok.TokenType.Type);

            type_annotation = self.parsePrimitiveTypeAnnotation();
            try parameters.append(ast.AstNode{ .FunctionParameter = ast.FunctionParameter{
                .ident = heap_ident,
                .inferred_type = type_annotation,
            } });
        }

        try self.matchNextAndAdvance(tok.TokenType.RPAREN);

        return parameters;
    }

    fn parseArrayLiteral(self: *Parser) error{ OutOfMemory, UnexpectedToken }!ast.AstNode {
        const lbracket_token = self.currentToken();
        const elements = try self.parseExpressionList(tok.TokenType.RBRACKET);

        return ast.AstNode{ .ArrayLiteral = ast.ArrayLiteral{
            .token = lbracket_token,
            .elements = elements,
        } };
    }

    fn parseExpressionList(self: *Parser, end: tok.TokenType) error{ OutOfMemory, UnexpectedToken }!std.ArrayList(ast.AstNode) {
        var expressions = std.ArrayList(ast.AstNode).init(self.alloc);
        if (self.matchPeekTokenType(end)) {
            self.advance();
            return expressions;
        }

        self.advance();
        var expression = try self.parseExpression(Precedence.LOWEST);
        try expressions.append(expression);

        while (self.matchPeekTokenType(tok.TokenType.COMMA)) {
            self.advance();
            self.advance();
            expression = try self.parseExpression(Precedence.LOWEST);
            try expressions.append(expression);
        }

        try self.matchNextAndAdvance(end);

        return expressions;
    }

    fn parseFunctionCall(self: *Parser, function_ident: ast.AstNode) error{ OutOfMemory, UnexpectedToken }!ast.AstNode {
        const lparen_token = self.currentToken();
        const function_arguments = try self.parseExpressionList(tok.TokenType.RPAREN);

        const heap_func_ident = try self.alloc.create(ast.AstNode);
        heap_func_ident.* = function_ident;

        return ast.AstNode{ .FunctionCall = ast.FunctionCall{
            .token = lparen_token,
            .function = heap_func_ident,
            .arguments = function_arguments,
        } };
    }

    fn parseIndexExpression(self: *Parser, indexed_exp: ast.AstNode) error{ OutOfMemory, UnexpectedToken }!ast.AstNode {
        const lbracket_token = self.currentToken();
        if (self.matchPeekTokenType(tok.TokenType.RBRACKET)) {
            return error.UnexpectedToken;
        }
        self.advance();

        const indexing_expression = try self.parseExpression(Precedence.LOWEST);

        try self.matchNextAndAdvance(tok.TokenType.RBRACKET);

        const heap_indexed_exp = try self.alloc.create(ast.AstNode);
        heap_indexed_exp.* = indexed_exp;

        const heap_indexing_exp = try self.alloc.create(ast.AstNode);
        heap_indexing_exp.* = indexing_expression;

        return ast.AstNode{ .Index = ast.Index{
            .token = lbracket_token,
            .indexed_expression = heap_indexed_exp,
            .expression = heap_indexing_exp,
        } };
    }

    fn parsePrimitiveTypeAnnotation(self: *Parser) ast.Type {
        const token = self.currentToken();
        return ast.Type{
            .PrimitiveType = ast.PrimitiveType.fromLiteral(token.literal),
        };
    }

    fn parseWhileLoopStatement(self: *Parser) error{ OutOfMemory, UnexpectedToken }!ast.AstNode {
        const while_token = self.currentToken();
        self.advance();

        const condition_expression = try self.parseExpression(Precedence.LOWEST);
        const heap_condition_expression = try self.alloc.create(ast.AstNode);
        heap_condition_expression.* = condition_expression;

        try self.matchNextAndAdvance(tok.TokenType.LBRACE);

        const block_statements = try self.parseBlockStatements();
        const heap_block_statements = try self.alloc.create(ast.AstNode);
        heap_block_statements.* = block_statements;

        return ast.AstNode{ .WhileLoopStatement = ast.WhileLoopStatement{
            .token = while_token,
            .condition = heap_condition_expression,
            .statements = heap_block_statements,
        } };
    }

    fn parseAssignmentStatement(self: *Parser) error{ OutOfMemory, UnexpectedToken }!ast.AstNode {
        const identifier = try self.parseIdentifier();
        self.advance();
        //skip equal sign
        self.advance();

        const expression = try self.parseExpression(Precedence.LOWEST);

        const heap_identifier = try self.alloc.create(ast.AstNode);
        heap_identifier.* = identifier;
        const heap_expression = try self.alloc.create(ast.AstNode);
        heap_expression.* = expression;

        try self.matchNextAndAdvance(tok.TokenType.SEMICOLON);

        return ast.AstNode{
            .AssignmentStatement = ast.AssignmentStatement{
                .token = identifier.Identifier.token,
                .name = heap_identifier,
                .expression = heap_expression,
            },
        };
    }
};

const lex = @import("lexer.zig");
const TestHelpers = struct {
    pub fn lex_tokens(lexer: *lex.Lexer, allocator: std.mem.Allocator) !std.ArrayList(tok.Token) {
        var lexed_tokens = std.ArrayList(tok.Token).init(allocator);

        while (true) {
            const cur_token = try lexer.next_token();
            try lexed_tokens.append(cur_token);
            if (cur_token.token_type == tok.TokenType.EOF) {
                break;
            }
        }
        return lexed_tokens;
    }

    pub fn test_let_statement(stmt: *const ast.AstNode, name: []const u8) !void {
        switch (stmt.*) {
            .LetStatement => |ls| {
                try std.testing.expectEqualStrings(ls.token.literal, "let");
                try std.testing.expectEqualStrings(ls.name.Identifier.value, name);
                try std.testing.expectEqualStrings(ls.name.Identifier.token.literal, name);
            },
            else => std.debug.panic("Expected let statement, got {s}", .{@tagName(stmt.*)}),
        }
    }

    pub fn test_parse_errors(parser: *Parser) !void {
        if (parser.errors().*.items.len > 0) {
            std.debug.print("Errors:\n", .{});
            for (parser.errors().*.items, 1..) |e, i| {
                std.debug.print("  {d}: {s}\n", .{ i, e });
            }
            return error.ParserHadErrors;
        }
    }

    pub fn test_literal_expression(expression: *const ast.AstNode, expected: anytype) !void {
        switch (@TypeOf(expected)) {
            i64, comptime_int => return TestHelpers.test_integer_literal(expression, @as(i64, expected)),
            f64, comptime_float => return TestHelpers.test_float_literal(expression, @as(f64, expected)),
            bool => return TestHelpers.test_boolean_literal(expression, expected),
            []const u8 => {
                if (expression.* == .StringLiteral) {
                    return TestHelpers.test_string_literal(expression, expected);
                }
                if (expression.* == .Identifier) {
                    return TestHelpers.test_identifier(expression, expected);
                }
                if (expression.* == .FunctionParameter) {
                    return TestHelpers.test_identifier(expression.FunctionParameter.ident, expected);
                }
                unreachable;
            },
            else => {
                std.debug.print("Found type {any}\n", .{@TypeOf(expected)});
                unreachable;
            },
        }
    }

    pub fn test_infix_expression(expression: *const ast.Infix, left: anytype, right: anytype, operator: ast.BinaryOp) !void {
        try std.testing.expectEqualStrings(operator.toString(), expression.operator.toString());
        try TestHelpers.test_literal_expression(expression.left, left);
        try TestHelpers.test_literal_expression(expression.right, right);
    }

    pub fn test_integer_literal(expression: *const ast.AstNode, expected: i64) !void {
        try std.testing.expect(expression.* == .IntegerLiteral);
        try std.testing.expectEqual(expression.*.IntegerLiteral.value, expected);
    }

    pub fn test_float_literal(expression: *const ast.AstNode, expected: f64) !void {
        try std.testing.expect(expression.* == .FloatLiteral);
        try std.testing.expectEqual(expression.*.FloatLiteral.value, expected);
    }

    pub fn test_string_literal(expression: *const ast.AstNode, expected: []const u8) !void {
        try std.testing.expect(expression.* == .StringLiteral);
        try std.testing.expectEqualStrings(expression.*.StringLiteral.value, expected);
    }

    pub fn test_boolean_literal(expression: *const ast.AstNode, expected: bool) !void {
        try std.testing.expect(expression.* == .BooleanLiteral);
        try std.testing.expectEqual(expected, expression.*.BooleanLiteral.value);
    }

    pub fn test_identifier(expression: *const ast.AstNode, expected: []const u8) !void {
        try std.testing.expect(expression.* == .Identifier);
        try std.testing.expectEqualStrings(expression.*.Identifier.value, expected);
        try std.testing.expectEqualStrings(expression.*.Identifier.token.literal, expected);
    }
};

test "Test parse let statement" {
    const Value = union(enum) {
        integer: i64,
        string: []const u8,
    };
    const TestCase = struct {
        input: []const u8,
        expected_identifier: []const u8,
        expected_value: Value,
    };
    const tests = [_]TestCase{
        .{
            .input = "let x: int = 5",
            .expected_identifier = "x",
            .expected_value = Value{ .integer = 5 },
        },
        .{
            .input = "let y = 10",
            .expected_identifier = "y",
            .expected_value = Value{ .integer = 10 },
        },
        .{
            .input = "let foobar = y",
            .expected_identifier = "foobar",
            .expected_value = Value{ .string = "y" },
        },
        .{
            .input = "let barbaz: string = \"str\"",
            .expected_identifier = "barbaz",
            .expected_value = Value{ .string = "str" },
        },
        //TODO: still need to implement function literal parsing
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var lexer = lex.Lexer.init(test_case.input, allocator);
        var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
        const slice_tokens = try lexed_tokens.toOwnedSlice();

        var parser = try Parser.init(slice_tokens, allocator);

        const program = try parser.parse();
        try TestHelpers.test_parse_errors(&parser);

        try std.testing.expectEqual(1, program.Program.program.items.len);
        try TestHelpers.test_let_statement(&program.Program.program.items[0], test_case.expected_identifier);

        const expression = program.Program.program.items[0].LetStatement.expression;
        switch (test_case.expected_value) {
            .integer => |v| try TestHelpers.test_literal_expression(expression, v),
            .string => |v| try TestHelpers.test_literal_expression(expression, v),
        }
    }
}

test "Test parse identifier" {
    const input = "foobar;";
    const expected: []const u8 = "foobar";

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var lexer = lex.Lexer.init(input, allocator);
    var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
    const slice_tokens = try lexed_tokens.toOwnedSlice();

    var parser = try Parser.init(slice_tokens, allocator);
    const program = try parser.parse();
    try TestHelpers.test_parse_errors(&parser);
    try std.testing.expectEqual(1, program.Program.program.items.len);

    const expression = program.Program.program.items[0].ExpressionStatement.expression;
    try TestHelpers.test_literal_expression(expression, expected);
}

test "Test parse string literals" {
    const input = "\"Hello world!\"";
    const expected: []const u8 = "Hello world!";

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var lexer = lex.Lexer.init(input, allocator);
    var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
    const slice_tokens = try lexed_tokens.toOwnedSlice();

    var parser = try Parser.init(slice_tokens, allocator);
    const program = try parser.parse();
    try TestHelpers.test_parse_errors(&parser);
    try std.testing.expectEqual(1, program.Program.program.items.len);

    const expression = program.Program.program.items[0].ExpressionStatement.expression;
    try TestHelpers.test_literal_expression(expression, expected);
}

test "Parse numeric expressions" {
    const Value = union(enum) {
        integer: i64,
        float: f64,
    };
    const TestCase = struct {
        input: []const u8,
        expected: Value,
    };
    const tests = [_]TestCase{
        .{ .input = "5;", .expected = Value{ .integer = 5 } },
        .{ .input = "134.965", .expected = Value{ .float = 134.965 } },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var lexer = lex.Lexer.init(test_case.input, allocator);
        var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
        const slice_tokens = try lexed_tokens.toOwnedSlice();

        var parser = try Parser.init(slice_tokens, allocator);
        const program = try parser.parse();
        try TestHelpers.test_parse_errors(&parser);
        try std.testing.expectEqual(1, program.Program.program.items.len);

        const expression = program.Program.program.items[0].ExpressionStatement.expression;
        switch (test_case.expected) {
            inline else => |v| try TestHelpers.test_literal_expression(expression, v),
        }
    }
}

test "Parse prefix expressions" {
    const Value = union(enum) {
        integer: i64,
        boolean: bool,
    };
    const TestCase = struct {
        input: []const u8,
        operator: ast.UnaryOp,
        value: Value,
    };
    const tests = [_]TestCase{
        .{ .input = "!5", .operator = ast.UnaryOp.Negation, .value = Value{ .integer = 5 } },
        .{ .input = "-10", .operator = ast.UnaryOp.Minus, .value = Value{ .integer = 10 } },
        .{ .input = "!true", .operator = ast.UnaryOp.Negation, .value = Value{ .boolean = true } },
        .{ .input = "!false", .operator = ast.UnaryOp.Negation, .value = Value{ .boolean = false } },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var lexer = lex.Lexer.init(test_case.input, allocator);
        var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
        const slice_tokens = try lexed_tokens.toOwnedSlice();

        var parser = try Parser.init(slice_tokens, allocator);
        const program = try parser.parse();
        try TestHelpers.test_parse_errors(&parser);

        const expression = program.Program.program.items[0].ExpressionStatement.expression.Prefix;
        try std.testing.expectEqualStrings(expression.operator.toString(), test_case.operator.toString());
        switch (test_case.value) {
            inline else => |v| try TestHelpers.test_literal_expression(expression.right, v),
        }
    }
}

test "Parse boolean expression" {
    const TestCase = struct {
        input: []const u8,
        value: bool,
    };
    const tests = [_]TestCase{
        .{ .input = "true;", .value = true },
        .{ .input = "false;", .value = false },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var lexer = lex.Lexer.init(test_case.input, allocator);
        var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
        const slice_tokens = try lexed_tokens.toOwnedSlice();

        var parser = try Parser.init(slice_tokens, allocator);
        const program = try parser.parse();
        try TestHelpers.test_parse_errors(&parser);

        const expression = program.Program.program.items[0].ExpressionStatement.expression;
        try TestHelpers.test_literal_expression(expression, test_case.value);
    }
}

test "Parse infix expression" {
    const Value = union(enum) {
        integer: i64,
        boolean: bool,
    };
    const TestCase = struct {
        input: []const u8,
        operator: ast.BinaryOp,
        left: Value,
        right: Value,
    };
    const tests = [_]TestCase{
        .{ .input = "5 + 5;", .operator = ast.BinaryOp.Plus, .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "5 - 5;", .operator = ast.BinaryOp.Minus, .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "5 * 5;", .operator = ast.BinaryOp.Multiply, .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "5 / 5;", .operator = ast.BinaryOp.Divide, .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "5 > 5;", .operator = ast.BinaryOp.Greater, .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "5 >= 5;", .operator = ast.BinaryOp.GreaterEqual, .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "5 < 5;", .operator = ast.BinaryOp.Less, .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "5 <= 5;", .operator = ast.BinaryOp.LessEqual, .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "5 == 5;", .operator = ast.BinaryOp.EqualEqual, .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "5 != 5;", .operator = ast.BinaryOp.NotEqual, .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "true == true;", .operator = ast.BinaryOp.EqualEqual, .left = Value{ .boolean = true }, .right = Value{ .boolean = true } },
        .{ .input = "true != true;", .operator = ast.BinaryOp.NotEqual, .left = Value{ .boolean = true }, .right = Value{ .boolean = true } },
        .{ .input = "true == false;", .operator = ast.BinaryOp.EqualEqual, .left = Value{ .boolean = true }, .right = Value{ .boolean = false } },
        .{ .input = "true != false;", .operator = ast.BinaryOp.NotEqual, .left = Value{ .boolean = true }, .right = Value{ .boolean = false } },
        .{ .input = "false == false;", .operator = ast.BinaryOp.EqualEqual, .left = Value{ .boolean = false }, .right = Value{ .boolean = false } },
        .{ .input = "false != false;", .operator = ast.BinaryOp.NotEqual, .left = Value{ .boolean = false }, .right = Value{ .boolean = false } },
    };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var lexer = lex.Lexer.init(test_case.input, allocator);
        var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
        const slice_tokens = try lexed_tokens.toOwnedSlice();

        var parser = try Parser.init(slice_tokens, allocator);
        const program = try parser.parse();
        try TestHelpers.test_parse_errors(&parser);
        try std.testing.expectEqual(1, program.Program.program.items.len);

        const expression = program.Program.program.items[0].ExpressionStatement.expression.Infix;

        switch (test_case.left) {
            inline else => |l| switch (test_case.right) {
                inline else => |r| try TestHelpers.test_infix_expression(&expression, l, r, test_case.operator),
            },
        }
    }
}

test "Parse if expressions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = "if x<y { x }";
    const left_exp: []const u8 = "x";
    const right_exp: []const u8 = "y";

    var lexer = lex.Lexer.init(input, allocator);
    var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
    const slice_tokens = try lexed_tokens.toOwnedSlice();

    var parser = try Parser.init(slice_tokens, allocator);
    const program = try parser.parse();
    try TestHelpers.test_parse_errors(&parser);
    try std.testing.expectEqual(1, program.Program.program.items.len);

    const expression = program.Program.program.items[0].ExpressionStatement.expression.If;
    try TestHelpers.test_infix_expression(&expression.condition.Infix, left_exp, right_exp, ast.BinaryOp.Less);
    try std.testing.expectEqual(1, expression.consequence.BlockStatement.statements.items.len);

    const consequence = expression.consequence.BlockStatement.statements.items[0].ExpressionStatement;
    try TestHelpers.test_identifier(consequence.expression, "x");
    try std.testing.expect(expression.alternative == null);
}

test "Parse function literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = "fn foo(x: float, y: float) float { x+y; }";
    const left_exp: []const u8 = "x";
    const right_exp: []const u8 = "y";
    const fn_name: []const u8 = "foo";

    var lexer = lex.Lexer.init(input, allocator);
    var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
    const slice_tokens = try lexed_tokens.toOwnedSlice();

    var parser = try Parser.init(slice_tokens, allocator);
    const program = try parser.parse();
    try TestHelpers.test_parse_errors(&parser);
    try std.testing.expectEqual(1, program.Program.program.items.len);

    const function_literal = program.Program.program.items[0].FunctionLiteral;

    try std.testing.expectEqual(2, function_literal.parameters.items.len);
    try std.testing.expectEqualStrings(fn_name, function_literal.name.?);
    try TestHelpers.test_literal_expression(function_literal.parameters.items[0].FunctionParameter.ident, left_exp);
    try TestHelpers.test_literal_expression(function_literal.parameters.items[1].FunctionParameter.ident, right_exp);
    try std.testing.expectEqual(1, function_literal.body.BlockStatement.statements.items.len);
    try TestHelpers.test_infix_expression(&function_literal.body.BlockStatement.statements.items[0].ExpressionStatement.expression.Infix, left_exp, right_exp, ast.BinaryOp.Plus);
}

test "Parse function parameters" {
    const TestCase = struct {
        input: []const u8,
        parameters: [][]const u8,

        pub fn strSlice(comptime strings: []const []const u8) [][]const u8 {
            const str: [][]const u8 = @constCast(strings);
            return str;
        }
    };
    const tests = [_]TestCase{
        .{ .input = "let foo = fn() void {};", .parameters = &[_][]const u8{} },
        .{ .input = "let foo = fn(x: int) void {};", .parameters = TestCase.strSlice(&.{"x"}) },
        .{ .input = "let foo = fn(x: float, y: string) void {};", .parameters = TestCase.strSlice(&.{ "x", "y" }) },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var lexer = lex.Lexer.init(test_case.input, allocator);
        var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
        const slice_tokens = try lexed_tokens.toOwnedSlice();

        var parser = try Parser.init(slice_tokens, allocator);
        const program = try parser.parse();

        try TestHelpers.test_parse_errors(&parser);
        try std.testing.expectEqual(1, program.Program.program.items.len);

        const function_literal = program.Program.program.items[0].LetStatement.expression.FunctionLiteral;
        try std.testing.expectEqual(test_case.parameters.len, function_literal.parameters.items.len);

        for (test_case.parameters, 0..) |param, i| {
            try TestHelpers.test_literal_expression(&function_literal.parameters.items[i], param);
        }
    }
}

test "Parse array literals" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = "[1, 2 * 2, 3 + 3]";

    var lexer = lex.Lexer.init(input, allocator);
    var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
    const slice_tokens = try lexed_tokens.toOwnedSlice();

    var parser = try Parser.init(slice_tokens, allocator);
    const program = try parser.parse();
    try TestHelpers.test_parse_errors(&parser);
    try std.testing.expectEqual(1, program.Program.program.items.len);

    const array_literal = &program.Program.program.items[0].ExpressionStatement.expression.ArrayLiteral;
    try std.testing.expectEqual(3, array_literal.elements.items.len);
    try TestHelpers.test_integer_literal(&array_literal.elements.items[0], 1);
}

test "Parse function call" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = "add(1, 2 * 3, 4 + 5)";

    var lexer = lex.Lexer.init(input, allocator);
    var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
    const slice_tokens = try lexed_tokens.toOwnedSlice();

    var parser = try Parser.init(slice_tokens, allocator);
    const program = try parser.parse();
    try TestHelpers.test_parse_errors(&parser);
    try std.testing.expectEqual(1, program.Program.program.items.len);

    const function_call = &program.Program.program.items[0].ExpressionStatement.expression.FunctionCall;
    try TestHelpers.test_identifier(function_call.function, "add");
    try std.testing.expectEqual(3, function_call.arguments.items.len);
    const function_call_args = function_call.arguments.items;
    try TestHelpers.test_literal_expression(&function_call_args[0], 1);
    try TestHelpers.test_infix_expression(&function_call_args[1].Infix, 2, 3, ast.BinaryOp.Multiply);
    try TestHelpers.test_infix_expression(&function_call_args[2].Infix, 4, 5, ast.BinaryOp.Plus);
}

test "Parse index expression" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = "myArray[1 + 1]";

    var lexer = lex.Lexer.init(input, allocator);
    var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
    const slice_tokens = try lexed_tokens.toOwnedSlice();

    var parser = try Parser.init(slice_tokens, allocator);
    const program = try parser.parse();
    try TestHelpers.test_parse_errors(&parser);
    try std.testing.expectEqual(1, program.Program.program.items.len);

    const index_expression = &program.Program.program.items[0].ExpressionStatement.expression.Index;
    try TestHelpers.test_identifier(index_expression.indexed_expression, "myArray");
    try TestHelpers.test_infix_expression(&index_expression.expression.Infix, 1, 1, ast.BinaryOp.Plus);
}

test "Parse return statement" {
    const Value = union(enum) {
        integer: i64,
        float: f64,
        string: []const u8,
    };
    const TestCase = struct {
        input: []const u8,
        expected_value: Value,
    };
    const tests = [_]TestCase{
        .{ .input = "return 5;", .expected_value = Value{ .integer = 5 } },
        .{ .input = "return 10.1;", .expected_value = Value{ .float = 10.1 } },
        .{ .input = "return foobar;", .expected_value = Value{ .string = "foobar" } },
        //TODO: test anonymous function returns
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var lexer = lex.Lexer.init(test_case.input, allocator);
        var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
        const slice_tokens = try lexed_tokens.toOwnedSlice();

        var parser = try Parser.init(slice_tokens, allocator);
        const program = try parser.parse();
        try TestHelpers.test_parse_errors(&parser);
        try std.testing.expectEqual(1, program.Program.program.items.len);
        switch (test_case.expected_value) {
            inline else => |v| try TestHelpers.test_literal_expression(program.Program.program.items[0].ReturnStatement.return_value, v),
        }
    }
}

test "Parse while loop statement" {
    const TestCase = struct {
        input: []const u8,
        left_cond_exp: []const u8,
        right_cond_exp: []const u8,
        n_statements: usize,
    };
    const tests = [_]TestCase{
        .{ .input = "while x<y { let a: int = 1; }", .left_cond_exp = "x", .right_cond_exp = "y", .n_statements = 1 },
        .{ .input = "while x>y { let a: int = 2; let b: int = 3; }", .left_cond_exp = "x", .right_cond_exp = "y", .n_statements = 2 },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var lexer = lex.Lexer.init(test_case.input, allocator);
        var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
        const slice_tokens = try lexed_tokens.toOwnedSlice();

        var parser = try Parser.init(slice_tokens, allocator);
        const program = try parser.parse();
        try TestHelpers.test_parse_errors(&parser);

        try std.testing.expectEqual(1, program.Program.program.items.len);
        const statement = program.Program.program.items[0].WhileLoopStatement;
        try TestHelpers.test_literal_expression(statement.condition.Infix.left, test_case.left_cond_exp);
        try TestHelpers.test_literal_expression(statement.condition.Infix.right, test_case.right_cond_exp);
        try std.testing.expectEqual(test_case.n_statements, statement.statements.BlockStatement.statements.items.len);
    }
}

test "Parse assigment statement" {
    const Value = union(enum) {
        integer: i64,
        string: []const u8,
    };
    const TestCase = struct {
        input: []const u8,
        expected_identifier: []const u8,
        expected_value: Value,
    };
    const tests = [_]TestCase{
        .{
            .input = "x = 5;",
            .expected_identifier = "x",
            .expected_value = Value{ .integer = 5 },
        },
        .{
            .input = "y = 10;",
            .expected_identifier = "y",
            .expected_value = Value{ .integer = 10 },
        },
        .{
            .input = "foobar = y;",
            .expected_identifier = "foobar",
            .expected_value = Value{ .string = "y" },
        },
        .{
            .input = "barbaz = \"str\";",
            .expected_identifier = "barbaz",
            .expected_value = Value{ .string = "str" },
        },
        //TODO: still need to implement function literal parsing
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var lexer = lex.Lexer.init(test_case.input, allocator);
        var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
        const slice_tokens = try lexed_tokens.toOwnedSlice();

        var parser = try Parser.init(slice_tokens, allocator);

        const program = try parser.parse();
        try TestHelpers.test_parse_errors(&parser);

        try std.testing.expectEqual(1, program.Program.program.items.len);
        try TestHelpers.test_identifier(program.Program.program.items[0].AssignmentStatement.name, test_case.expected_identifier);

        const expression = program.Program.program.items[0].AssignmentStatement.expression;
        switch (test_case.expected_value) {
            .integer => |v| try TestHelpers.test_literal_expression(expression, v),
            .string => |v| try TestHelpers.test_literal_expression(expression, v),
        }
    }
}
