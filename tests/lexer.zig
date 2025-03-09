const std = @import("std");
const zonkey = @import("zonkey");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};

const TestCase = struct {
    expected_type: zonkey.token.TokenType,
    expected_literal: []const u8,
};

test "Test symbols" {
    const input = "[]=]==;()";

    const alloc = gpa.allocator();
    var lex = zonkey.lexer.Lexer.init(input, alloc);
    defer lex.errors_list.deinit();

    const tests = [_]TestCase{
        .{ .expected_type = zonkey.token.TokenType.LBRACKET, .expected_literal = "[" },
        .{ .expected_type = zonkey.token.TokenType.RBRACKET, .expected_literal = "]" },
        .{ .expected_type = zonkey.token.TokenType.EQUAL, .expected_literal = "=" },
        .{ .expected_type = zonkey.token.TokenType.RBRACKET, .expected_literal = "]" },
        .{ .expected_type = zonkey.token.TokenType.EQUAL_EQUAL, .expected_literal = "==" },
        .{ .expected_type = zonkey.token.TokenType.SEMICOLON, .expected_literal = ";" },
        .{ .expected_type = zonkey.token.TokenType.LPAREN, .expected_literal = "(" },
        .{ .expected_type = zonkey.token.TokenType.RPAREN, .expected_literal = ")" },
    };

    for (tests) |test_case| {
        const token = lex.next_token();
        try std.testing.expectEqual(test_case.expected_type, token.token_type);
        try std.testing.expectEqualStrings(token.literal, test_case.expected_literal);
    }
}

test "Test end of file" {
    const input = "";

    const alloc = gpa.allocator();
    var lex = zonkey.lexer.Lexer.init(input, alloc);
    defer lex.errors_list.deinit();

    const token = lex.next_token();
    try std.testing.expectEqual(zonkey.token.TokenType.EOF, token.token_type);
}
