const std = @import("std");
const ast = @import("ast.zig");
const tok = @import("token.zig");

const prefixParserFn = fn (*Parser) anyerror!ast.ExpressionNode;
const infixParserFn = fn (*Parser, ast.ExpressionNode) anyerror!ast.ExpressionNode;

const Precedence = enum(u8) {
    LOWEST = 1,
    EQUALS, // ==
    LESSGREATER, // <, >
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
        .GREATER => Precedence.LESSGREATER,
        .PLUS => Precedence.SUM,
        .MINUS => Precedence.SUM,
        .SLASH => Precedence.PRODUCT,
        .ASTERISK => Precedence.PRODUCT,
        .LPAREN => Precedence.CALL,
        .LBRACKET => Precedence.INDEX,
        else => Precedence.LOWEST,
    };
}

const Parser = struct {
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
        try self.infix_fns.put(tok.TokenType.LESS, Parser.parseInfixExpression);
        try self.infix_fns.put(tok.TokenType.EQUAL_EQUAL, Parser.parseInfixExpression);
        try self.infix_fns.put(tok.TokenType.BANG_EQUAL, Parser.parseInfixExpression);
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

    fn matchNextAndAdvance(self: *Parser, tt: tok.TokenType) !bool {
        if (!self.matchPeekTokenType(tt)) {
            try self.peekError(tt);
            return false;
        }
        self.advance();
        return true;
    }

    fn peekError(self: *Parser, tt: tok.TokenType) !void {
        const token = self.peekToken();
        // FIXME: how and when do I dealloc this?
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

    fn parseletError(self: *Parser) !void {
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

    pub fn parse(self: *Parser) !*ast.Program {
        var program = ast.Program.init(self.alloc);

        while (!self.matchCurrentTokenType(tok.TokenType.EOF)) {
            const stmt = try self.parseStatement();

            if (stmt) |s| {
                try program.addStatement(s);
            }
            self.advance();
        }

        return &program;
    }

    fn parseStatement(self: *Parser) !?ast.StatementNode {
        return switch (self.currentToken().token_type) {
            tok.TokenType.LET => try self.parseLetStatement(),
            tok.TokenType.RETURN => unreachable,
            else => try self.parseExpressionStatement(),
        };
    }

    fn parseLetStatement(self: *Parser) !?ast.StatementNode {
        // already know current token is a let token
        const let_token = self.currentToken();
        // expect next to be an identifier
        if (!try self.matchNextAndAdvance(tok.TokenType.IDENT)) {
            return null;
        }
        const ident_token = self.currentToken();

        //expect = sign
        if (!try self.matchNextAndAdvance(tok.TokenType.EQUAL)) {
            return null;
        }
        self.advance();

        const expression = try self.parseExpression(Precedence.LOWEST) orelse {
            return error.NullExpressionParsed;
        };

        if (self.matchPeekTokenType(tok.TokenType.SEMICOLON)) {
            self.advance();
        }

        return ast.StatementNode.createLetStatement(ast.Identifier{ .token = ident_token, .value = ident_token.literal }, let_token, expression);
    }

    fn parseExpressionStatement(self: *Parser) !?ast.StatementNode {
        const token = self.currentToken();
        //FIXME: this here is still optional
        const expression = try self.parseExpression(Precedence.LOWEST) orelse {
            return error.NullExpressionParsed;
        };

        if (self.matchPeekTokenType(tok.TokenType.SEMICOLON)) {
            self.advance();
        }

        return ast.StatementNode{ .ExpressionStatement = ast.ExpressionStatement{
            .token = token,
            .expression = expression,
        } };
    }

    fn parseExpression(self: *Parser, prec: Precedence) !?ast.ExpressionNode {
        const prefix_parselet = self.prefix_fns.get(self.currentToken().token_type) orelse {
            try self.parseletError();
            return null;
        };
        var left_expr = try prefix_parselet(self);

        while (!self.matchPeekTokenType(tok.TokenType.SEMICOLON) and @intFromEnum(prec) < @intFromEnum(self.peekPrecedence())) {
            const infix_parselet = self.infix_fns.get(self.peekToken().token_type) orelse {
                // FIXME: this has to be for the peek token
                try self.parseletError();
                return left_expr;
            };
            self.advance();
            left_expr = try infix_parselet(self, left_expr);
        }

        return left_expr;
    }

    fn parseIntegerLiteral(self: *Parser) !ast.ExpressionNode {
        const token = self.currentToken();
        return ast.ExpressionNode{ .IntegerLiteral = ast.IntegerLiteral{
            .token = token,
            .value = try std.fmt.parseInt(i64, token.literal, 10),
        } };
    }

    fn parseIdentifier(self: *Parser) !ast.ExpressionNode {
        const token = self.currentToken();
        return ast.ExpressionNode{ .Identifier = ast.Identifier{
            .token = token,
            .value = token.literal,
        } };
    }

    fn parseStringLiteral(self: *Parser) !ast.ExpressionNode {
        const token = self.currentToken();
        return ast.ExpressionNode{ .StringLiteral = ast.StringLiteral{
            .token = token,
            .value = token.literal,
        } };
    }

    fn parsePrefixExpression(self: *Parser) !ast.ExpressionNode {
        // current token is e.g. - or !
        const token = self.currentToken();
        // move the pointer to the expression
        self.advance();
        // parse the expression
        const expression = try self.parseExpression(Precedence.PREFIX) orelse {
            //FIXME: somewhere top level the parser needs to be synchronized, since the syntax is broken at this point
            return error.NullExpressionParsed;
        };

        // We have to allocate it on the heap due to recursive structure: ExpressionNode -> Prefix -> ExpressionNode
        const heapExpressionNode = try self.alloc.create(ast.ExpressionNode);
        heapExpressionNode.* = expression;

        return ast.ExpressionNode{ .Prefix = ast.Prefix{
            .operator = token.literal,
            .token = token,
            .right = heapExpressionNode,
        } };
    }

    fn parseBoolean(self: *Parser) !ast.ExpressionNode {
        const token = self.currentToken();
        return ast.ExpressionNode{ .BooleanLiteral = ast.BooleanLiteral{ .token = token, .value = self.matchCurrentTokenType(tok.TokenType.TRUE) } };
    }

    fn parseGroupedExpression(self: *Parser) !ast.ExpressionNode {
        // skip to the next token
        self.advance();

        const grouped_expression = try self.parseExpression(Precedence.LOWEST) orelse {
            return error.NullExpressionParsed;
        };

        // grouped expression must end with a matching closing ")"
        if (try self.matchNextAndAdvance(tok.TokenType.RPAREN)) {
            return error.UnexpectedToken;
        }

        return grouped_expression;
    }

    fn parseInfixExpression(self: *Parser, left: ast.ExpressionNode) !ast.ExpressionNode {
        // the operator of the infix expression, e.g. +, -, *, /, etc.
        const token = self.currentToken();
        const cur_precedence = self.currentPrecedence();
        // skip to the token starting the next expression
        self.advance();
        // parse right expression
        const expression = try self.parseExpression(cur_precedence) orelse {
            return error.NullExpressionParsed;
        };

        // have to allocate on the heap due to recursive structure
        const heapRightExpressionNode = try self.alloc.create(ast.ExpressionNode);
        heapRightExpressionNode.* = expression;
        const heapLeftExpressionNode = try self.alloc.create(ast.ExpressionNode);
        heapLeftExpressionNode.* = left;

        return ast.ExpressionNode{ .Infix = ast.Infix{
            .token = token,
            .operator = token.literal,
            .left = heapLeftExpressionNode,
            .right = heapRightExpressionNode,
        } };
    }

    fn parseIfExpression(self: *Parser) !ast.ExpressionNode {
        const if_token = self.currentToken();
        // advance to the start of the expression
        self.advance();
        const if_expression = try self.parseExpression(Precedence.LOWEST) orelse {
            return error.NullExpressionParsed;
        };
        const heap_if_expression = try self.alloc.create(ast.ExpressionNode);
        heap_if_expression.* = if_expression;

        if (!try self.matchNextAndAdvance(tok.TokenType.LBRACE)) {
            return error.UnexpectedToken;
        }

        const consequence = try self.parseBlockStatements();

        if (self.matchPeekTokenType(tok.TokenType.ELSE)) {
            self.advance();

            if (!try self.matchNextAndAdvance(tok.TokenType.LBRACE)) {
                return error.UnexpectedToken;
            }
            const alternative = try self.parseBlockStatements();

            return ast.ExpressionNode{
                .If = ast.If{
                    .token = if_token,
                    .condition = heap_if_expression,
                    .consequence = consequence,
                    .alternative = alternative,
                },
            };
        }

        return ast.ExpressionNode{ .If = ast.If{
            .token = if_token,
            .condition = heap_if_expression,
            .consequence = consequence,
            .alternative = null,
        } };
    }

    fn parseBlockStatements(self: *Parser) !ast.BlockStatement {
        const lbrace_token = self.currentToken();
        self.advance();

        var block_statements = ast.BlockStatement.init(lbrace_token, self.alloc);

        while (!self.matchCurrentTokenType(tok.TokenType.RBRACE) and !self.matchCurrentTokenType(tok.TokenType.EOF)) {
            const statement = try self.parseStatement() orelse {
                // FIXME: should record an error and skip to the next statement
                return error.NullStatementParsed;
            };
            try block_statements.addStatement(statement);
            // we're looking at the last token after each parselet, so skipping it
            self.advance();
        }

        return block_statements;
    }

    fn parseFunctionLiteral(self: *Parser) !ast.ExpressionNode {
        const fn_token = self.currentToken();

        if (!try self.matchNextAndAdvance(tok.TokenType.LPAREN)) {
            return error.UnexpectedToken;
        }

        const parameters = try self.parseFunctionParameters();

        if (!try self.matchNextAndAdvance(tok.TokenType.LBRACE)) {
            return error.UnexpectedToken;
        }

        const fn_body_statements = try self.parseBlockStatements();

        return ast.ExpressionNode{ .FunctionLiteral = ast.FunctionLiteral{
            .token = fn_token,
            .parameters = parameters,
            .body = fn_body_statements,
        } };
    }

    fn parseFunctionParameters(self: *Parser) !std.ArrayList(ast.Identifier) {
        var parameters = std.ArrayList(ast.Identifier).init(self.alloc);
        if (self.matchPeekTokenType(tok.TokenType.RPAREN)) {
            self.advance();
            return parameters;
        }
        if (!try self.matchNextAndAdvance(tok.TokenType.IDENT)) {
            return error.UnexpectedToken;
        }

        var ident = ast.Identifier{ .token = self.currentToken(), .value = self.currentToken().literal };
        try parameters.append(ident);

        while (self.matchPeekTokenType(tok.TokenType.COMMA)) {
            self.advance();
            if (!try self.matchNextAndAdvance(tok.TokenType.IDENT)) {
                return error.UnexpectedToken;
            }

            ident = ast.Identifier{ .token = self.currentToken(), .value = self.currentToken().literal };
            try parameters.append(ident);
        }

        if (!try self.matchNextAndAdvance(tok.TokenType.RPAREN)) {
            return error.UnexpectedToken;
        }

        return parameters;
    }

    fn parseArrayLiteral(self: *Parser) !ast.ExpressionNode {
        const lbracket_token = self.currentToken();
        const elements = try self.parseExpressionList();

        return ast.ExpressionNode{ .ArrayLiteral = ast.ArrayLiteral{
            .token = lbracket_token,
            .elements = elements,
        } };
    }

    fn parseExpressionList(self: *Parser) !std.ArrayList(ast.ExpressionNode) {
        var expressions = std.ArrayList(ast.ExpressionNode).init(self.alloc);
        if (self.matchPeekTokenType(tok.TokenType.RBRACKET)) {
            self.advance();
            return expressions;
        }

        self.advance();
        var expression = try self.parseExpression(Precedence.LOWEST) orelse {
            return error.NullExpressionParsed;
        };
        try expressions.append(expression);

        while (self.matchPeekTokenType(tok.TokenType.COMMA)) {
            self.advance();
            self.advance();
            expression = try self.parseExpression(Precedence.LOWEST) orelse {
                return error.NullExpressionParsed;
            };
            try expressions.append(expression);
        }

        if (!try self.matchNextAndAdvance(tok.TokenType.RBRACKET)) {
            return error.UnexpectedToken;
        }

        return expressions;
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

    pub fn test_let_statement(stmt: *const ast.StatementNode, name: []const u8) !void {
        switch (stmt.*) {
            ast.Statement.LetStatement => |ls| {
                try std.testing.expectEqualStrings(ls.token.literal, "let");
                try std.testing.expectEqualStrings(ls.name.value, name);
                try std.testing.expectEqualStrings(ls.name.token.literal, name);
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

    pub fn test_literal_expression(expression: *const ast.ExpressionNode, expected: anytype) !void {
        switch (@TypeOf(expected)) {
            i64 => return TestHelpers.test_integer_literal(expression, @as(i64, expected)),
            bool => return TestHelpers.test_boolean_literal(expression, expected),
            []const u8 => {
                if (expression.* == .StringLiteral) {
                    return TestHelpers.test_string_literal(expression, expected);
                }
                if (expression.* == .Identifier) {
                    return TestHelpers.test_identifier(expression, expected);
                }
            },
            else => {
                std.debug.print("Found type {any}\n", .{@TypeOf(expected)});
                unreachable;
            },
        }
    }

    pub fn test_infix_expression(expression: *const ast.Infix, left: anytype, right: anytype, operator: []const u8) !void {
        try std.testing.expectEqualStrings(operator, expression.operator);
        try TestHelpers.test_literal_expression(expression.left, left);
        try TestHelpers.test_literal_expression(expression.right, right);
    }

    pub fn test_integer_literal(expression: *const ast.ExpressionNode, expected: i64) !void {
        if (expression.* != .IntegerLiteral) {
            return error.ExpectedIntegerLiteral;
        }
        try std.testing.expect(expression.* == .IntegerLiteral);
        try std.testing.expectEqual(expression.*.IntegerLiteral.value, expected);
    }

    pub fn test_string_literal(expression: *const ast.ExpressionNode, expected: []const u8) !void {
        try std.testing.expect(expression.* == .StringLiteral);
        try std.testing.expectEqualStrings(expression.*.StringLiteral.value, expected);
    }

    pub fn test_boolean_literal(expression: *const ast.ExpressionNode, expected: bool) !void {
        try std.testing.expect(expression.* == .BooleanLiteral);
        try std.testing.expectEqual(expected, expression.*.BooleanLiteral.value);
    }

    pub fn test_identifier(expression: *const ast.ExpressionNode, expected: []const u8) !void {
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
        .{ .input = "let x = 5", .expected_identifier = "x", .expected_value = Value{ .integer = 5 } },
        .{ .input = "let y = 10", .expected_identifier = "y", .expected_value = Value{ .integer = 10 } },
        .{ .input = "let foobar = y", .expected_identifier = "foobar", .expected_value = Value{ .string = "y" } },
        .{ .input = "let barbaz = \"str\"", .expected_identifier = "barbaz", .expected_value = Value{ .string = "str" } },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var lexer = lex.Lexer.init(test_case.input, allocator);
        var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
        const slice_tokens = try lexed_tokens.toOwnedSlice();

        var parser = try Parser.init(slice_tokens, allocator);

        const program = parser.parse() catch |err| {
            //FIXME: stop ignoring the error once the error diagnostics are complete
            TestHelpers.test_parse_errors(&parser) catch {};
            return err;
        };
        try std.testing.expectEqual(1, program.program.items.len);
        try TestHelpers.test_let_statement(&program.program.items[0], test_case.expected_identifier);

        const expression = &program.program.items[0].LetStatement.value;
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
    const program = parser.parse() catch |err| {
        //FIXME: stop ignoring the error once the error diagnostics are complete
        TestHelpers.test_parse_errors(&parser) catch {};
        return err;
    };
    try std.testing.expectEqual(1, program.program.items.len);

    const expression = program.program.items[0].ExpressionStatement.expression;
    try TestHelpers.test_literal_expression(&expression, expected);
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
    const program = parser.parse() catch |err| {
        //FIXME: stop ignoring the error once the error diagnostics are complete
        TestHelpers.test_parse_errors(&parser) catch {};
        return err;
    };
    try std.testing.expectEqual(1, program.program.items.len);

    const expression = program.program.items[0].ExpressionStatement.expression;
    try TestHelpers.test_literal_expression(&expression, expected);
}

test "Parse integer expressions" {
    const input = "5;";
    const expected: i64 = 5;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var lexer = lex.Lexer.init(input, allocator);
    var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
    const slice_tokens = try lexed_tokens.toOwnedSlice();

    var parser = try Parser.init(slice_tokens, allocator);
    const program = parser.parse() catch |err| {
        //FIXME: stop ignoring the error once the error diagnostics are complete
        TestHelpers.test_parse_errors(&parser) catch {};
        return err;
    };
    try std.testing.expectEqual(1, program.program.items.len);

    const expression = program.program.items[0].ExpressionStatement.expression;
    try TestHelpers.test_literal_expression(&expression, expected);
}

test "Parse prefix expressions" {
    const Value = union(enum) {
        integer: i64,
        boolean: bool,
    };
    const TestCase = struct {
        input: []const u8,
        operator: []const u8,
        value: Value,
    };
    const tests = [_]TestCase{
        .{ .input = "!5", .operator = "!", .value = Value{ .integer = 5 } },
        .{ .input = "-10", .operator = "-", .value = Value{ .integer = 10 } },
        .{ .input = "!true", .operator = "!", .value = Value{ .boolean = true } },
        .{ .input = "!false", .operator = "!", .value = Value{ .boolean = false } },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var lexer = lex.Lexer.init(test_case.input, allocator);
        var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
        const slice_tokens = try lexed_tokens.toOwnedSlice();

        var parser = try Parser.init(slice_tokens, allocator);
        const program = parser.parse() catch |err| {
            //FIXME: stop ignoring the error once the error diagnostics are complete
            TestHelpers.test_parse_errors(&parser) catch {};
            return err;
        };

        const expression = &program.program.items[0].ExpressionStatement.expression.Prefix;
        try std.testing.expectEqualStrings(expression.*.operator, test_case.operator);
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
        const program = parser.parse() catch |err| {
            //FIXME: stop ignoring the error once the error diagnostics are complete
            TestHelpers.test_parse_errors(&parser) catch {};
            return err;
        };

        const expression = &program.program.items[0].ExpressionStatement.expression;
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
        operator: []const u8,
        left: Value,
        right: Value,
    };
    const tests = [_]TestCase{
        .{ .input = "5 + 5;", .operator = "+", .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "5 - 5;", .operator = "-", .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "5 * 5;", .operator = "*", .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "5 / 5;", .operator = "/", .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "5 > 5;", .operator = ">", .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "5 < 5;", .operator = "<", .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "5 == 5;", .operator = "==", .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "5 != 5;", .operator = "!=", .left = Value{ .integer = 5 }, .right = Value{ .integer = 5 } },
        .{ .input = "true == true;", .operator = "==", .left = Value{ .boolean = true }, .right = Value{ .boolean = true } },
        .{ .input = "true != true;", .operator = "!=", .left = Value{ .boolean = true }, .right = Value{ .boolean = true } },
        .{ .input = "true == false;", .operator = "==", .left = Value{ .boolean = true }, .right = Value{ .boolean = false } },
        .{ .input = "true != false;", .operator = "!=", .left = Value{ .boolean = true }, .right = Value{ .boolean = false } },
        .{ .input = "false == false;", .operator = "==", .left = Value{ .boolean = false }, .right = Value{ .boolean = false } },
        .{ .input = "false != false;", .operator = "!=", .left = Value{ .boolean = false }, .right = Value{ .boolean = false } },
    };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var lexer = lex.Lexer.init(test_case.input, allocator);
        var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
        const slice_tokens = try lexed_tokens.toOwnedSlice();

        var parser = try Parser.init(slice_tokens, allocator);
        const program = parser.parse() catch |err| {
            //FIXME: stop ignoring the error once the error diagnostics are complete
            TestHelpers.test_parse_errors(&parser) catch {};
            return err;
        };
        try std.testing.expectEqual(1, program.program.items.len);

        const expression = &program.program.items[0].ExpressionStatement.expression.Infix;

        switch (test_case.left) {
            inline else => |l| switch (test_case.right) {
                inline else => |r| try TestHelpers.test_infix_expression(expression, l, r, test_case.operator),
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
    const program = parser.parse() catch |err| {
        //FIXME: stop ignoring the error once the error diagnostics are complete
        TestHelpers.test_parse_errors(&parser) catch {};
        return err;
    };
    try std.testing.expectEqual(1, program.program.items.len);

    const expression = &program.program.items[0].ExpressionStatement.expression.If;
    try TestHelpers.test_infix_expression(&expression.condition.Infix, left_exp, right_exp, "<");
    try std.testing.expectEqual(1, expression.consequence.statements.items.len);

    const consequence = &expression.consequence.statements.items[0].ExpressionStatement;
    try TestHelpers.test_identifier(&consequence.expression, "x");
    try std.testing.expect(expression.alternative == null);
}

test "Parse function literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = "fn(x, y) { x+y; }";
    const left_exp: []const u8 = "x";
    const right_exp: []const u8 = "y";

    var lexer = lex.Lexer.init(input, allocator);
    var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
    const slice_tokens = try lexed_tokens.toOwnedSlice();

    var parser = try Parser.init(slice_tokens, allocator);
    const program = parser.parse() catch |err| {
        //FIXME: stop ignoring the error once the error diagnostics are complete
        TestHelpers.test_parse_errors(&parser) catch {};
        return err;
    };
    try std.testing.expectEqual(1, program.program.items.len);

    const function_literal = &program.program.items[0].ExpressionStatement.expression.FunctionLiteral;

    try std.testing.expectEqual(2, function_literal.parameters.items.len);
    //FIXME: might actually have to store an ExpressionNode in the parameters list
    try TestHelpers.test_literal_expression(&ast.ExpressionNode{ .Identifier = function_literal.parameters.items[0] }, left_exp);
    try TestHelpers.test_literal_expression(&ast.ExpressionNode{ .Identifier = function_literal.parameters.items[1] }, right_exp);
    try std.testing.expectEqual(1, function_literal.body.statements.items.len);
    try TestHelpers.test_infix_expression(&function_literal.body.statements.items[0].ExpressionStatement.expression.Infix, left_exp, right_exp, "+");
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
        .{ .input = "fn(){};", .parameters = &[_][]const u8{} },
        .{ .input = "fn(x){};", .parameters = TestCase.strSlice(&.{"x"}) },
        .{ .input = "fn(x, y){};", .parameters = TestCase.strSlice(&.{ "x", "y" }) },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (tests) |test_case| {
        var lexer = lex.Lexer.init(test_case.input, allocator);
        var lexed_tokens = try TestHelpers.lex_tokens(&lexer, allocator);
        const slice_tokens = try lexed_tokens.toOwnedSlice();

        var parser = try Parser.init(slice_tokens, allocator);
        const program = parser.parse() catch |err| {
            //FIXME: stop ignoring the error once the error diagnostics are complete
            TestHelpers.test_parse_errors(&parser) catch {};
            return err;
        };
        try std.testing.expectEqual(1, program.program.items.len);

        const function_literal = &program.program.items[0].ExpressionStatement.expression.FunctionLiteral;
        try std.testing.expectEqual(test_case.parameters.len, function_literal.parameters.items.len);

        for (test_case.parameters, 0..) |param, i| {
            try TestHelpers.test_literal_expression(&ast.ExpressionNode{ .Identifier = function_literal.parameters.items[i] }, param);
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
    const program = parser.parse() catch |err| {
        //FIXME: stop ignoring the error once the error diagnostics are complete
        TestHelpers.test_parse_errors(&parser) catch {};
        return err;
    };
    try std.testing.expectEqual(1, program.program.items.len);

    const array_literal = &program.program.items[0].ExpressionStatement.expression.ArrayLiteral;
    try std.testing.expectEqual(3, array_literal.elements.items.len);
    try TestHelpers.test_integer_literal(&array_literal.elements.items[0], 1);
}
