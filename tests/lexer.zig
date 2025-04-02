const std = @import("std");
const zonkey = @import("zonkey");
const TokenType = zonkey.token.TokenType;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};

const TestCase = struct {
    expected_type: zonkey.token.TokenType,
    expected_literal: []const u8,
};

test "Test symbols" {
    const input = "[ ] = ] == ; ( ) , + { } != ! / * < > - : <= >=";

    const alloc = gpa.allocator();
    var lex = zonkey.lexer.Lexer.init(input, alloc);
    defer lex.deinit();

    const tests = [_]TestCase{
        .{ .expected_type = TokenType.LBRACKET, .expected_literal = "[" },
        .{ .expected_type = TokenType.RBRACKET, .expected_literal = "]" },
        .{ .expected_type = TokenType.EQUAL, .expected_literal = "=" },
        .{ .expected_type = TokenType.RBRACKET, .expected_literal = "]" },
        .{ .expected_type = TokenType.EQUAL_EQUAL, .expected_literal = "==" },
        .{ .expected_type = TokenType.SEMICOLON, .expected_literal = ";" },
        .{ .expected_type = TokenType.LPAREN, .expected_literal = "(" },
        .{ .expected_type = TokenType.RPAREN, .expected_literal = ")" },
        .{ .expected_type = TokenType.COMMA, .expected_literal = "," },
        .{ .expected_type = TokenType.PLUS, .expected_literal = "+" },
        .{ .expected_type = TokenType.LBRACE, .expected_literal = "{" },
        .{ .expected_type = TokenType.RBRACE, .expected_literal = "}" },
        .{ .expected_type = TokenType.BANG_EQUAL, .expected_literal = "!=" },
        .{ .expected_type = TokenType.BANG, .expected_literal = "!" },
        .{ .expected_type = TokenType.SLASH, .expected_literal = "/" },
        .{ .expected_type = TokenType.ASTERISK, .expected_literal = "*" },
        .{ .expected_type = TokenType.LESS, .expected_literal = "<" },
        .{ .expected_type = TokenType.GREATER, .expected_literal = ">" },
        .{ .expected_type = TokenType.MINUS, .expected_literal = "-" },
        .{ .expected_type = TokenType.COLON, .expected_literal = ":" },
        .{ .expected_type = TokenType.LESS_EQUAL, .expected_literal = "<=" },
        .{ .expected_type = TokenType.GREATER_EQUAL, .expected_literal = ">=" },
    };

    for (tests) |test_case| {
        const token = try lex.next_token();
        try std.testing.expectEqual(test_case.expected_type, token.token_type);
        try std.testing.expectEqualStrings(token.literal, test_case.expected_literal);
    }
}

test "Test end of file" {
    const input = "";

    const alloc = gpa.allocator();
    var lex = zonkey.lexer.Lexer.init(input, alloc);
    defer lex.deinit();

    const token = try lex.next_token();
    try std.testing.expectEqual(TokenType.EOF, token.token_type);
}

test "Test string literals" {
    const input = "\"some_text\"";
    const alloc = gpa.allocator();
    var lex = zonkey.lexer.Lexer.init(input, alloc);
    defer lex.deinit();

    const token = try lex.next_token();

    try std.testing.expectEqualStrings("some_text", token.literal);
    try std.testing.expectEqual(TokenType.STRING, token.token_type);
}

test "Test string literals error" {
    const input = "\"some_text";
    const alloc = gpa.allocator();
    var lex = zonkey.lexer.Lexer.init(input, alloc);
    defer lex.deinit();

    _ = try lex.next_token();

    try std.testing.expectEqual(1, lex.errors_list.items.len);
}

test "Test keywords" {
    const input = "fn let if else return true false int float string bool";
    const tests = [_]TestCase{
        .{ .expected_type = TokenType.FUNCTION, .expected_literal = "fn" },
        .{ .expected_type = TokenType.LET, .expected_literal = "let" },
        .{ .expected_type = TokenType.IF, .expected_literal = "if" },
        .{ .expected_type = TokenType.ELSE, .expected_literal = "else" },
        .{ .expected_type = TokenType.RETURN, .expected_literal = "return" },
        .{ .expected_type = TokenType.TRUE, .expected_literal = "true" },
        .{ .expected_type = TokenType.FALSE, .expected_literal = "false" },
        .{ .expected_type = TokenType.Type, .expected_literal = "int" },
        .{ .expected_type = TokenType.Type, .expected_literal = "float" },
        .{ .expected_type = TokenType.Type, .expected_literal = "string" },
        .{ .expected_type = TokenType.Type, .expected_literal = "bool" },
    };
    const alloc = gpa.allocator();
    var lex = zonkey.lexer.Lexer.init(input, alloc);
    defer lex.deinit();

    for (tests) |test_case| {
        const token = try lex.next_token();

        try std.testing.expectEqual(test_case.expected_type, token.token_type);
        try std.testing.expectEqualStrings(test_case.expected_literal, token.literal);
    }
}

test "Test random identifiers" {
    const input = "testvar aplusb";
    const tests = [_]TestCase{
        .{ .expected_type = TokenType.IDENT, .expected_literal = "testvar" },
        .{ .expected_type = TokenType.IDENT, .expected_literal = "aplusb" },
    };

    const alloc = gpa.allocator();
    var lex = zonkey.lexer.Lexer.init(input, alloc);
    defer lex.deinit();

    for (tests) |test_case| {
        const token = try lex.next_token();

        try std.testing.expectEqual(test_case.expected_type, token.token_type);
        try std.testing.expectEqualStrings(test_case.expected_literal, token.literal);
    }
}

test "Test numeric literals" {
    const input = "12345 34179 324.421";
    const tests = [_]TestCase{
        .{ .expected_type = TokenType.INT, .expected_literal = "12345" },
        .{ .expected_type = TokenType.INT, .expected_literal = "34179" },
        .{ .expected_type = TokenType.FLOAT, .expected_literal = "324.421" },
    };

    const alloc = gpa.allocator();
    var lex = zonkey.lexer.Lexer.init(input, alloc);
    defer lex.deinit();

    for (tests) |test_case| {
        const token = try lex.next_token();

        try std.testing.expectEqual(test_case.expected_type, token.token_type);
        try std.testing.expectEqualStrings(test_case.expected_literal, token.literal);
    }
}
