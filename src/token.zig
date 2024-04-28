const std = @import("std");

pub const Token = union(enum) {
    illegal,
    eof,
    ident: []const u8,
    int: []const u8,
    assign,
    plus,
    minus,
    bang,
    slash,
    asterisk,
    lt,
    gt,
    eq,
    not_eq,
    comma,
    semicolon,
    lparen,
    rparen,
    lbrace,
    rbrace,
    function,
    let,
    if_token,
    else_token,
    return_token,
    true_token,
    false_token,

    pub fn isKeyword(ident: []const u8) ?Token {
        const map = std.ComptimeStringMap(Token, .{
            .{ "let", .let },
            .{ "fn", .function },
            .{ "if", .if_token },
            .{ "true", .true_token },
            .{ "false", .false_token },
            .{ "return", .return_token },
            .{ "else", .else_token },
        });
        return map.get(ident);
    }
};
