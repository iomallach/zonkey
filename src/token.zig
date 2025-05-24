const std = @import("std");

pub const TokenType = enum {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT,
    INT,
    FLOAT,

    TYPE,
    UNIT,

    // Operators
    EQUAL,
    PLUS,
    BANG,
    MINUS,
    SLASH,
    ASTERISK,
    LESS,
    LESS_EQUAL,
    GREATER,
    GREATER_EQUAL,
    EQUAL_EQUAL,
    BANG_EQUAL,

    RETURN_TYPE,

    COMMA,
    SEMICOLON,
    COLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,

    // Keywords
    FUNCTION,
    LET,
    IF,
    ELSE,
    RETURN,
    WHILE,

    // Builtins
    PRINT,

    // Bool
    TRUE,
    FALSE,

    // String literal
    STRING,
};

pub const TokenSpan = struct {
    start: usize,
    end: usize,
    line_number: usize,
    source_chunk: []const u8,
};

pub const Token = struct {
    token_type: TokenType,
    literal: []const u8,
    span: TokenSpan,
};

const keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "fn", TokenType.FUNCTION },
    .{ "let", TokenType.LET },
    .{ "if", TokenType.IF },
    .{ "else", TokenType.ELSE },
    .{ "return", TokenType.RETURN },
    .{ "true", TokenType.TRUE },
    .{ "false", TokenType.FALSE },
    .{ "int", TokenType.TYPE },
    .{ "float", TokenType.TYPE },
    .{ "string", TokenType.TYPE },
    .{ "bool", TokenType.TYPE },
    .{ "while", TokenType.WHILE },
});

const builtins = std.StaticStringMap(TokenType).initComptime(.{
    .{ "print", TokenType.PRINT },
});

pub fn map_identifier(keyword: []const u8) TokenType {
    return keywords.get(keyword) orelse builtins.get(keyword) orelse TokenType.IDENT;
}
