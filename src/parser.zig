const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const program = @import("program.zig");
const ast = @import("ast.zig");

const Parser = struct {
    const Self = @This();

    lexer: lexer.Lexer,
    curToken: token.Token,
    peekToken: token.Token,

    pub fn init(l: lexer.Lexer) Parser {
        var p: Parser = .{ .lexer = l, .curToken = .illegal, .peekToken = .illegal };
        p.nextToken();
        p.nextToken();
        return p;
    }

    fn nextToken(self: *Self) void {
        self.curToken = self.peekToken;
        self.peekToken = self.lexer.nextToken();
    }

    pub fn parseProgram(self: *Self) !program.Program {
        var prog = try program.Program.init();

        while (self.curToken != .eof) {
            const stmt = self.parseStatement();
            if (stmt) |s| {
                try prog.statements.append(s);
            }
            self.nextToken();
        }

        return prog;
    }

    fn parseStatement(self: *Self) ?ast.Statement {
        switch (self.curToken) {
            .let => return self.parseLetStatement(),
            else => return null,
        }
    }

    fn parseLetStatement(self: *Self) ?ast.Statement {
        if (!self.expectPeekIsIdent()) {
            return null;
        }
        const name = parseIdentifier(self.curToken);
        // std.log.warn("Parsing let satement: {s}", .{name});
        const ident = ast.Identifier.init(name);

        if (!self.expectPeek(.assign)) {
            self.nextToken();
        }

        while (!self.curTokenIs(.semicolon)) {
            self.nextToken();
        }

        const stmt = .{ .letStatement = .{ .identifier = ident, .expression = .{ .identifier = ident } } };

        return stmt;
    }

    fn parseIdentifier(t: token.Token) []const u8 {
        // std.log.warn("Parsing identifier: {s}", .{@tagName(t)});
        switch (t) {
            .ident => |i| return i,
            else => return "",
        }
    }

    fn curTokenIs(self: *Self, t: token.Token) bool {
        return std.mem.eql(u8, @tagName(self.curToken), @tagName(t));
    }

    fn peekTokenIs(self: *Self, t: token.Token) bool {
        return std.mem.eql(u8, @tagName(self.peekToken), @tagName(t));
    }

    fn expectPeekIsIdent(self: *Self) bool {
        if (std.mem.eql(u8, @tagName(self.peekToken), @tagName(.ident))) {
            self.nextToken();
            return true;
        } else {
            return false;
        }
    }

    fn expectPeek(self: *Self, t: token.Token) bool {
        if (self.peekTokenIs(t)) {
            self.nextToken();
            return true;
        } else {
            return false;
        }
    }
};

test "test let statement" {
    const assert = std.debug.assert;

    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    const l = lexer.Lexer.init(input);
    var p = Parser.init(l);

    var prog = try p.parseProgram();

    assert(prog.statements.items.len == 3);

    const tests = [_]struct {
        expected: []const u8,
    }{
        .{ .expected = "x" },
        .{ .expected = "y" },
        .{ .expected = "foobar" },
    };

    for (prog.statements.items, 0..) |stmt, i| {
        const tt = tests[i];
        assert(std.mem.eql(u8, @tagName(stmt), @tagName(ast.Statement.letStatement)));
        assert(std.mem.eql(u8, stmt.letStatement.identifier.identifier, tt.expected));
    }
}
