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

    pub fn id(self: Self) usize {
        switch (self) {
            .illegal => return 0,
            .eof => return 1,
            .ident => return 2,
            .int => return 4,
            else => return 5,
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
        }
    }
};
