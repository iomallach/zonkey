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

    // fn find_end_of_line(self: *Lexer) usize {
    //     const newline: u8 = '\n';
    //     for (self.chars[self.position..], self.position..) |c, offset| {
    //         if ((c == newline) || (c == 0)) {
    //             return self.position + offset + 1;
    //         }
    //     }
    //
    //     return self.chars.len;
    // }

    fn token_span(self: *Lexer, start: usize, end: usize) tok.TokenSpan {
        const cur_line_end = self.find_end_of_line();
        return .{
            .start = start - self.cur_line_start,
            .end = end - self.cur_line_start,
            .line_number = self.cur_line,
            .source_chunk = self.chars[self.cur_line_start..cur_line_end],
        };
    }

    pub fn next_token(self: *Lexer) tok.Token {
        var token: tok.Token = undefined;
        var span: tok.TokenSpan = undefined;

        self.maybe_increment_line();

        switch (self.char()) {
            '[' => {
                span = self.token_span(self.position, self.position);
                token = tok.Token{ .token_type = tok.TokenType.LBRACKET, .literal = self.chars[self.position .. self.position + 1], .span = span };
            },
            ']' => {
                span = self.token_span(self.position, self.position);
                token = tok.Token{ .token_type = tok.TokenType.RBRACKET, .literal = self.chars[self.position .. self.position + 1], .span = span };
            },
            0 => {
                span = self.token_span(0, 0);
                token = tok.Token{ .token_type = tok.TokenType.EOF, .literal = "", .span = span };
            },
            else => {
                @panic("Not implemented");
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

    fn consume_while(self: *Lexer, f: fn (u8) bool) LiteralAndSpan {
        if (!f(self.char())) {
            return .{ .literal = "", .span = .{ .start = 0, .end = 0, .line_number = 0, .source_chunk = "" } };
        }

        const start = self.position;
        for (f(self.char())) |_| {
            self.advance();
        }
        const span = self.token_span(start, self.position);

        return .{ .literal = self.chars[start..self.position], .span = span };
    }
};
