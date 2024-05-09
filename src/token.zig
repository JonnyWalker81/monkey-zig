const std = @import("std");

pub const Token = union(enum) {
    const Self = @This();

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
    lbracket,
    rbracket,
    function,
    let,
    if_token,
    else_token,
    return_token,
    true_token,
    false_token,
    string: []const u8,

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

    pub fn id(self: Self) usize {
        switch (self) {
            .illegal => return 0,
            .eof => return 1,
            .ident => return 2,
            .int => return 3,
            .assign => return 4,
            .plus => return 5,
            .minus => return 6,
            .bang => return 7,
            .slash => return 8,
            .asterisk => return 9,
            .lt => return 10,
            .gt => return 11,
            .eq => return 12,
            .not_eq => return 13,
            .comma => return 14,
            .semicolon => return 15,
            .lparen => return 16,
            .rparen => return 17,
            .lbrace => return 18,
            .rbrace => return 19,
            .function => return 20,
            .let => return 21,
            .if_token => return 22,
            .else_token => return 23,
            .return_token => return 24,
            .true_token => return 25,
            .false_token => return 26,
            .string => return 27,
            .lbracket => return 28,
            .rbracket => return 29,
        }
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .illegal => try writer.print("ILLEGAL", .{}),
            .eof => try writer.print("EOF", .{}),
            .ident => |i| try writer.print("IDENT: {s}", .{i}),
            .int => |i| try writer.print("INT: {s}", .{i}),
            .assign => try writer.print("=", .{}),
            .plus => try writer.print("+", .{}),
            .minus => try writer.print("-", .{}),
            .bang => try writer.print("!", .{}),
            .slash => try writer.print("/", .{}),
            .asterisk => try writer.print("*", .{}),
            .lt => try writer.print("<", .{}),
            .gt => try writer.print(">", .{}),
            .eq => try writer.print("==", .{}),
            .not_eq => try writer.print("!=", .{}),
            .comma => try writer.print(",", .{}),
            .semicolon => try writer.print(";", .{}),
            .lparen => try writer.print("(", .{}),
            .rparen => try writer.print(")", .{}),
            .lbrace => try writer.print("{{", .{}),
            .rbrace => try writer.print("}}", .{}),
            .function => try writer.print("FUNCTION", .{}),
            .let => try writer.print("LET", .{}),
            .if_token => try writer.print("IF", .{}),
            .else_token => try writer.print("ELSE", .{}),
            .return_token => try writer.print("RETURN", .{}),
            .true_token => try writer.print("TRUE", .{}),
            .false_token => try writer.print("FALSE", .{}),
            .string => |s| try writer.print("STRING: {s}", .{s}),
            .lbracket => try writer.print("[", .{}),
            .rbracket => try writer.print("]", .{}),
        }
    }
};
