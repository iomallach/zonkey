const tok = @import("token.zig");
const std = @import("std");
const allocator = std.mem.Allocator;

const LiteralAndSpan = struct { literal: []const u8, span: tok.TokenSpan };
const newline: u8 = 10;

pub const Lexer = struct {
    chars: []const u8,
    position: usize,
    cur_line: usize,
    cur_line_start: usize,

    errors_list: std.ArrayList([]const u8),

    pub fn init(chars: []const u8, alloc: std.mem.Allocator) Lexer {
        return .{
            .chars = chars,
            .position = 0,
            .cur_line = 1,
            .cur_line_start = 0,
            .errors_list = std.ArrayList([]const u8).init(alloc),
        };
    }

    pub fn deinit(self: *Lexer) void {
        self.errors_list.deinit();
    }

    pub fn errors(self: *Lexer) std.ArrayList[[]const u8] {
        return self.errors_list;
    }

    fn advance(self: *Lexer) void {
        self.position += 1;
    }

    fn char(self: *Lexer) u8 {
        if (self.position >= self.chars.len) {
            return 0;
        }
        return self.chars[self.position];
    }

    fn find_end_of_line(self: *Lexer) usize {
        var offset: usize = 0;
        while (self.position + offset < self.chars.len) {
            const c = self.chars[self.position + offset];
            if (std.mem.eql(u8, &[_]u8{c}, &[_]u8{newline}) or (std.mem.eql(u8, &[_]u8{c}, &[_]u8{0}))) {
                return self.position + offset + 1;
            }
            offset += 1;
        }

        return self.chars.len;
    }

    fn token_span(self: *Lexer, start: usize, end: usize) tok.TokenSpan {
        const cur_line_end = self.find_end_of_line();
        return .{
            .start = start - self.cur_line_start,
            .end = end - self.cur_line_start,
            .line_number = self.cur_line,
            .source_chunk = self.chars[self.cur_line_start..cur_line_end],
        };
    }

    fn slice_literal(self: *Lexer, to: usize) []const u8 {
        return self.chars[self.position .. self.position + to];
    }

    pub fn next_token(self: *Lexer) !tok.Token {
        var token: tok.Token = undefined;
        var span: tok.TokenSpan = undefined;

        self.maybe_increment_line();
        _ = self.consume_while(Lexer.is_whitespace);

        switch (self.char()) {
            '[' => {
                span = self.token_span(self.position, self.position);
                token = tok.Token{ .token_type = tok.TokenType.LBRACKET, .literal = self.slice_literal(1), .span = span };
            },
            ']' => {
                span = self.token_span(self.position, self.position);
                token = tok.Token{ .token_type = tok.TokenType.RBRACKET, .literal = self.slice_literal(1), .span = span };
            },
            '=' => {
                if (self.peek() == '=') {
                    span = self.token_span(self.position, self.position + 1);
                    token = tok.Token{ .token_type = tok.TokenType.EQUAL_EQUAL, .literal = self.slice_literal(2), .span = span };
                    self.advance();
                } else {
                    span = self.token_span(self.position, self.position + 1);
                    token = tok.Token{ .token_type = tok.TokenType.EQUAL, .literal = self.slice_literal(1), .span = span };
                }
            },
            ':' => {
                span = self.token_span(self.position, self.position);
                token = tok.Token{ .token_type = tok.TokenType.COLON, .literal = self.slice_literal(1), .span = span };
            },
            ';' => {
                span = self.token_span(self.position, self.position);
                token = tok.Token{ .token_type = tok.TokenType.SEMICOLON, .literal = self.slice_literal(1), .span = span };
            },
            '(' => {
                span = self.token_span(self.position, self.position);
                token = tok.Token{ .token_type = tok.TokenType.LPAREN, .literal = self.slice_literal(1), .span = span };
            },
            ')' => {
                span = self.token_span(self.position, self.position);
                token = tok.Token{ .token_type = tok.TokenType.RPAREN, .literal = self.slice_literal(1), .span = span };
            },
            ',' => {
                span = self.token_span(self.position, self.position);
                token = tok.Token{ .token_type = tok.TokenType.COMMA, .literal = self.slice_literal(1), .span = span };
            },
            '+' => {
                span = self.token_span(self.position, self.position);
                token = tok.Token{ .token_type = tok.TokenType.PLUS, .literal = self.slice_literal(1), .span = span };
            },
            '{' => {
                span = self.token_span(self.position, self.position);
                token = tok.Token{ .token_type = tok.TokenType.LBRACE, .literal = self.slice_literal(1), .span = span };
            },
            '}' => {
                span = self.token_span(self.position, self.position);
                token = tok.Token{ .token_type = tok.TokenType.RBRACE, .literal = self.slice_literal(1), .span = span };
            },
            '!' => {
                if (self.peek() == '=') {
                    span = self.token_span(self.position, self.position + 1);
                    token = tok.Token{ .token_type = tok.TokenType.BANG_EQUAL, .literal = self.slice_literal(2), .span = span };
                    self.advance();
                } else {
                    span = self.token_span(self.position, self.position);
                    token = tok.Token{ .token_type = tok.TokenType.BANG, .literal = self.slice_literal(1), .span = span };
                }
            },
            '/' => {
                span = self.token_span(self.position, self.position);
                token = tok.Token{ .token_type = tok.TokenType.SLASH, .literal = self.slice_literal(1), .span = span };
            },
            '*' => {
                span = self.token_span(self.position, self.position);
                token = tok.Token{ .token_type = tok.TokenType.ASTERISK, .literal = self.slice_literal(1), .span = span };
            },
            '<' => {
                if (self.peek() == '=') {
                    span = self.token_span(self.position, self.position + 1);
                    token = tok.Token{ .token_type = tok.TokenType.LESS_EQUAL, .literal = self.slice_literal(2), .span = span };
                    self.advance();
                } else {
                    span = self.token_span(self.position, self.position);
                    token = tok.Token{ .token_type = tok.TokenType.LESS, .literal = self.slice_literal(1), .span = span };
                }
            },
            '>' => {
                if (self.peek() == '=') {
                    span = self.token_span(self.position, self.position + 1);
                    token = tok.Token{ .token_type = tok.TokenType.GREATER_EQUAL, .literal = self.slice_literal(2), .span = span };
                } else {
                    span = self.token_span(self.position, self.position);
                    token = tok.Token{ .token_type = tok.TokenType.GREATER, .literal = self.slice_literal(1), .span = span };
                }
            },
            '-' => {
                span = self.token_span(self.position, self.position);
                token = tok.Token{ .token_type = tok.TokenType.MINUS, .literal = self.slice_literal(1), .span = span };
            },
            '"' => {
                // skip the '"' in the literal
                self.advance();
                const literal_and_span = self.consume_while(Lexer.not_quote_and_end);
                token = tok.Token{ .token_type = tok.TokenType.STRING, .literal = literal_and_span.literal, .span = literal_and_span.span };
                if (self.char() != '"') {
                    //FIXME: error message is broken, see go implementation
                    try self.errors_list.append("FIXME: need an actual error");
                }
                // not skipping " at the end due to the advance after switch
            },
            0 => {
                span = self.token_span(self.chars.len, self.chars.len);
                token = tok.Token{ .token_type = tok.TokenType.EOF, .literal = "", .span = span };
            },
            else => {
                if (self.is_letter()) {
                    const literal_and_span = self.consume_while(Lexer.is_letter);
                    const token_type = tok.map_identifier(literal_and_span.literal);
                    return tok.Token{ .token_type = token_type, .literal = literal_and_span.literal, .span = literal_and_span.span };
                } else if (self.is_integer()) {
                    const literal_and_span = self.consume_while(Lexer.is_numeric);
                    if (std.mem.indexOf(u8, literal_and_span.literal, ".")) |index| {
                        _ = index;
                        return tok.Token{ .token_type = tok.TokenType.FLOAT, .literal = literal_and_span.literal, .span = literal_and_span.span };
                    }
                    return tok.Token{ .token_type = tok.TokenType.INT, .literal = literal_and_span.literal, .span = literal_and_span.span };
                } else {
                    span = self.token_span(self.position, self.position);
                    token = tok.Token{ .token_type = tok.TokenType.ILLEGAL, .literal = self.slice_literal(1), .span = span };
                }
            },
        }

        self.advance();
        return token;
    }

    fn maybe_increment_line(self: *Lexer) void {
        if (self.char() == '\n') {
            self.cur_line += 1;

            if (self.peek() != 0) {
                self.cur_line_start = self.position + 1;
            }
        }
    }

    fn peek(self: *Lexer) u8 {
        if (self.position + 1 >= self.chars.len) {
            return 0;
        }
        return self.chars[self.position + 1];
    }

    fn consume_while(self: *Lexer, f: fn (*Lexer) bool) LiteralAndSpan {
        if (!f(self)) {
            return .{ .literal = "", .span = .{ .start = 0, .end = 0, .line_number = 0, .source_chunk = "" } };
        }

        const start = self.position;
        while (f(self)) {
            self.advance();
        }
        const span = self.token_span(start, self.position);

        return .{ .literal = self.chars[start..self.position], .span = span };
    }

    fn not_quote_and_end(self: *Lexer) bool {
        const c = self.char();
        return (c != '"') and (c != 0);
    }

    fn is_letter(self: *Lexer) bool {
        const c = self.char();
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c == '_');
    }

    fn is_integer(self: *Lexer) bool {
        const c = self.char();
        return (c >= '0' and c <= '9');
    }

    fn is_next_integer(self: *Lexer) bool {
        const c = self.peek();
        return (c >= '0' and c <= '9');
    }

    fn is_valid_dot_in_number(self: *Lexer) bool {
        const c = self.char();
        return (c == '.' and self.is_next_integer());
    }

    fn is_numeric(self: *Lexer) bool {
        return (self.is_integer() or self.is_valid_dot_in_number());
    }

    fn is_whitespace(self: *Lexer) bool {
        const c = self.char();
        return (c == ' ') or (c == '\t') or (c == '\n') or (c == '\r');
    }
};

//FIXME: doesn't cover end of file
test "Test find_end_of_line" {
    const input = "text\ntext\n";
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var lex = Lexer.init(input, alloc);
    defer lex.deinit();

    const eol = lex.find_end_of_line();

    try std.testing.expectEqual(5, eol);
}
