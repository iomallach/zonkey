pub const TokenType = enum {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT,
    INT,

    // Operators
    EQUAL,
    PLUS,
    BANG,
    MINUS,
    SLASH,
    ASTERISK,
    LESS,
    GREATER,
    EQUAL_EQUAL,
    BANG_EQUAL,

    COMMA,
    SEMICOLON,

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
