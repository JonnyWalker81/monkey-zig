const std = @import("std");
const token = @import("token.zig");

pub const Lexer = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    input: []u8,
    position: usize,
    readPosition: usize,
    ch: u8,

    pub fn init(allocator: std.mem.Allocator, input: []const u8) Lexer {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const ownedInput = arena.allocator().alloc(u8, input.len) catch {
            std.debug.panic("failed to allocate memory", .{});
        };
        @memcpy(ownedInput, input);
        var l = Lexer{
            .arena = arena,
            .input = ownedInput,
            .position = 0,
            .readPosition = 0,
            .ch = 0,
        };

        l.readChar();

        return l;
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }

    fn isLetter(ch: u8) bool {
        // std.log.warn("ch: {}", .{ch});
        return ('a' <= ch and ch <= 'z') or ('A' <= ch and ch <= 'Z') or ch == '_';
    }

    fn isDigit(ch: u8) bool {
        return '0' <= ch and ch <= '9';
    }

    fn readIdentifier(self: *Self) []const u8 {
        const position = self.position;
        while (isLetter(self.ch)) {
            self.readChar();
        }
        return self.input[position..self.position];
    }

    fn readNumber(self: *Self) []const u8 {
        const position = self.position;
        while (isDigit(self.ch)) {
            self.readChar();
        }

        return self.input[position..self.position];
    }

    fn isWhitespace(ch: u8) bool {
        switch (ch) {
            ' ', '\t', '\n', '\r' => return true,
            else => return false,
        }
    }

    fn skipWhitespace(self: *Self) void {
        while (isWhitespace(self.ch)) {
            self.readChar();
        }
    }

    pub fn nextToken(self: *Self) token.Token {
        self.skipWhitespace();

        const tok: token.Token = switch (self.ch) {
            '+' => .plus,
            '-' => .minus,
            '!' => blk: {
                if (self.peekChar() == '=') {
                    self.readChar();
                    break :blk .not_eq;
                } else {
                    break :blk .bang;
                }
            },
            '*' => .asterisk,
            '/' => .slash,
            '<' => .lt,
            '>' => .gt,
            '(' => .lparen,
            ')' => .rparen,
            '{' => .lbrace,
            '}' => .rbrace,
            ',' => .comma,
            ';' => .semicolon,
            '=' => blk: {
                if (self.peekChar() == '=') {
                    self.readChar();
                    break :blk .eq;
                } else {
                    break :blk .assign;
                }
            },
            '"' => {
                return .{ .string = self.readString() };
            },
            0 => .eof,
            else => {
                if (isLetter(self.ch)) {
                    const i = self.readIdentifier();
                    if (token.Token.isKeyword(i)) |t| {
                        return t;
                    }

                    return .{ .ident = i };
                } else if (isDigit(self.ch)) {
                    const n = self.readNumber();
                    return .{ .int = n };
                } else {
                    return .illegal;
                }
            },
        };

        self.readChar();

        return tok;
    }

    pub fn readString(self: *Self) []const u8 {
        self.readChar();
        const position = self.position;
        while (self.ch != '"' and self.ch != 0) {
            self.readChar();
        }

        self.readChar();

        return self.input[position .. self.position - 1];
    }

    pub fn readChar(self: *Self) void {
        if (self.readPosition >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.readPosition];
        }

        self.position = self.readPosition;
        self.readPosition += 1;
    }

    pub fn peekChar(self: *Self) u8 {
        if (self.readPosition >= self.input.len) {
            return 0;
        } else {
            return self.input[self.readPosition];
        }
    }
};

const expectEqualDeep = std.testing.expectEqualDeep;
const test_allocator = std.testing.allocator;
test "test next token" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\ 
        \\let add = fn(x, y) {
        \\   x + y;
        \\};
        \\
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\
        \\if (5 < 10) {
        \\   return true;
        \\} else {
        \\   return false;
        \\}
        \\
        \\10 == 10;
        \\10 != 9;
        \\"foobar"
        \\"foo bar"
    ;

    const tests = [_]struct {
        token: token.Token,
        expected: []const u8,
    }{
        .{ .token = .let, .expected = "let" },
        .{ .token = .{ .ident = "five" }, .expected = "five" },
        .{ .token = .assign, .expected = "=" },
        .{ .token = .{ .int = "5" }, .expected = "5" },
        .{ .token = .semicolon, .expected = ";" },
        .{ .token = .let, .expected = "let" },
        .{ .token = .{ .ident = "ten" }, .expected = "ten" },
        .{ .token = .assign, .expected = "=" },
        .{ .token = .{ .int = "10" }, .expected = "10" },
        .{ .token = .semicolon, .expected = ";" },
        .{ .token = .let, .expected = "let" },
        .{ .token = .{ .ident = "add" }, .expected = "add" },
        .{ .token = .assign, .expected = "=" },
        .{ .token = .function, .expected = "fn" },
        .{ .token = .lparen, .expected = "(" },
        .{ .token = .{ .ident = "x" }, .expected = "x" },
        .{ .token = .comma, .expected = "," },
        .{ .token = .{ .ident = "y" }, .expected = "y" },
        .{ .token = .rparen, .expected = ")" },
        .{ .token = .lbrace, .expected = "{" },
        .{ .token = .{ .ident = "x" }, .expected = "x" },
        .{ .token = .plus, .expected = "+" },
        .{ .token = .{ .ident = "y" }, .expected = "y" },
        .{ .token = .semicolon, .expected = ";" },
        .{ .token = .rbrace, .expected = "}" },
        .{ .token = .semicolon, .expected = ";" },
        .{ .token = .let, .expected = "let" },
        .{ .token = .{ .ident = "result" }, .expected = "result" },
        .{ .token = .assign, .expected = "=" },
        .{ .token = .{ .ident = "add" }, .expected = "add" },
        .{ .token = .lparen, .expected = "(" },
        .{ .token = .{ .ident = "five" }, .expected = "five" },
        .{ .token = .comma, .expected = "," },
        .{ .token = .{ .ident = "ten" }, .expected = "ten" },
        .{ .token = .rparen, .expected = ")" },
        .{ .token = .semicolon, .expected = ";" },
        .{ .token = .bang, .expected = "!" },
        .{ .token = .minus, .expected = "-" },
        .{ .token = .slash, .expected = "/" },
        .{ .token = .asterisk, .expected = "*" },
        .{ .token = .{ .int = "5" }, .expected = "5" },
        .{ .token = .semicolon, .expected = ";" },
        .{ .token = .{ .int = "5" }, .expected = "5" },
        .{ .token = .lt, .expected = "<" },
        .{ .token = .{ .int = "10" }, .expected = "10" },
        .{ .token = .gt, .expected = ">" },
        .{ .token = .{ .int = "5" }, .expected = "5" },
        .{ .token = .semicolon, .expected = ";" },
        .{ .token = .if_token, .expected = "if" },
        .{ .token = .lparen, .expected = "(" },
        .{ .token = .{ .int = "5" }, .expected = "5" },
        .{ .token = .lt, .expected = "<" },
        .{ .token = .{ .int = "10" }, .expected = "10" },
        .{ .token = .rparen, .expected = ")" },
        .{ .token = .lbrace, .expected = "{" },
        .{ .token = .return_token, .expected = "return" },
        .{ .token = .true_token, .expected = "true" },
        .{ .token = .semicolon, .expected = ";" },
        .{ .token = .rbrace, .expected = "}" },
        .{ .token = .else_token, .expected = "else" },
        .{ .token = .lbrace, .expected = "{" },
        .{ .token = .return_token, .expected = "return" },
        .{ .token = .false_token, .expected = "false" },
        .{ .token = .semicolon, .expected = ";" },
        .{ .token = .rbrace, .expected = "}" },
        .{ .token = .{ .int = "10" }, .expected = "10" },
        .{ .token = .eq, .expected = "==" },
        .{ .token = .{ .int = "10" }, .expected = "10" },
        .{ .token = .semicolon, .expected = ";" },
        .{ .token = .{ .int = "10" }, .expected = "10" },
        .{ .token = .not_eq, .expected = "==" },
        .{ .token = .{ .int = "9" }, .expected = "9" },
        .{ .token = .semicolon, .expected = ";" },
        .{ .token = .{ .string = "foobar" }, .expected = "foobar" },
        .{ .token = .{ .string = "foo bar" }, .expected = "foo bar" },
        .{ .token = .eof, .expected = "" },
    };

    var l = Lexer.init(test_allocator, input);
    defer l.deinit();

    for (tests) |t| {
        const tok = l.nextToken();
        // std.log.warn("{}", .{tok});
        try expectEqualDeep(tok, t.token);
    }
}
