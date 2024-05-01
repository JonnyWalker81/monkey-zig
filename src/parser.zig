const std = @import("std");
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const program = @import("program.zig");
const ast = @import("ast.zig");

// var gpa = std.heap.GeneralPurposeAllocator(.{}){};
// const allocator = gpa.allocator();
// var gpa = std.heap.GeneralPurposeAllocator(.{}){};
// var arena = std.heap.ArenaAllocator.init(gpa.allocator());

const prefixParserFn = *const fn (*Parser) ?*ast.Expression;
const infixParserFn = *const fn (*ast.Expression) ?*ast.Expression;

const Precedence = enum(u8) {
    lowest = 0,
    equals = 1,
    lessGreater = 2,
    sum = 3,
    product = 4,
    prefix = 5,
    call = 6,
};

pub const Parser = struct {
    const Self = @This();

    lexer: lexer.Lexer,
    curToken: token.Token,
    peekToken: token.Token,
    errors: ArrayListUnmanaged([]u8),
    prefixParseFns: AutoHashMapUnmanaged(usize, prefixParserFn),
    infixParseFns: AutoHashMapUnmanaged(usize, infixParserFn),
    arena: std.heap.ArenaAllocator,

    pub fn init(l: lexer.Lexer, allocator: std.mem.Allocator) Parser {
        var p: Parser = .{
            .lexer = l,
            .curToken = .illegal,
            .peekToken = .illegal,
            .errors = .{},
            .prefixParseFns = .{},
            .infixParseFns = .{},
            .arena = std.heap.ArenaAllocator.init(allocator),
        };

        var identToken: token.Token = .{ .ident = "" };
        p.registerPrefix(identToken.id(), parseIdentifierExpression) catch {
            std.debug.panic("failed to register prefix", .{});
        };

        var intToken: token.Token = .{ .int = "" };
        p.registerPrefix(intToken.id(), parseIntegerLiteral) catch {
            std.debug.panic("failed to register prefix", .{});
        };

        var bangToken: token.Token = .bang;
        p.registerPrefix(bangToken.id(), parsePrefixExpression) catch {
            std.debug.panic("failed to register prefix", .{});
        };

        var minusToken: token.Token = .minus;
        p.registerPrefix(minusToken.id(), parsePrefixExpression) catch {
            std.debug.panic("failed to register prefix", .{});
        };

        p.nextToken();
        p.nextToken();
        return p;
    }

    fn registerPrefix(self: *Self, id: usize, f: prefixParserFn) !void {
        try self.prefixParseFns.put(self.arena.allocator(), id, f);
    }

    fn registerInfix(self: *Self, id: usize, f: infixParserFn) !void {
        try self.infixParseFns.put(self.arena.allocator(), id, f);
    }

    fn nextToken(self: *Self) void {
        self.curToken = self.peekToken;
        self.peekToken = self.lexer.nextToken();
    }

    fn peekError(self: *Self, t: token.Token) void {
        // const msg = std.heap.sprintf(allocator, "expected next token to be {s}, got {s} instead", .{@tagName(t)}, .{@tagName(self.peekToken)});
        const msg = std.fmt.allocPrint(
            self.arena.allocator(),
            "expected next token to be {s}, got {s} instead",
            .{ @tagName(t), @tagName(self.peekToken) },
        ) catch {
            std.debug.panic("failed to format error message", .{});
        };
        self.errors.append(self.arena.allocator(), msg) catch {
            std.debug.panic("failed to append error message", .{});
        };
    }

    fn noPrefixParseFnError(self: *Self, t: token.Token) void {
        const msg = std.fmt.allocPrint(self.arena.allocator(), "no prefix parse function for {s} found", .{@tagName(t)}) catch {
            std.debug.panic("failed to format error message", .{});
        };
        self.errors.append(self.arena.allocator(), msg) catch {
            std.debug.panic("failed to append error message", .{});
        };
    }

    pub fn getErrors(self: *Self) []const []u8 {
        return self.errors.items;
    }

    pub fn parseProgram(self: *Self) program.Program {
        var prog = program.Program.init(self.arena.allocator());
        while (self.curToken != .eof) {
            const stmt = self.parseStatement();
            if (stmt) |s| {
                prog.statements.append(self.arena.allocator(), s) catch {
                    std.debug.panic("failed to append statement", .{});
                };
            }
            self.nextToken();
        }

        return prog;
    }

    fn parseStatement(self: *Self) ?*ast.Statement {
        switch (self.curToken) {
            .let => return self.parseLetStatement(),
            .return_token => return self.parseReturnStatement(),
            else => return parseExpressionStatement(self),
        }
    }

    fn parseExpressionStatement(self: *Self) ?*ast.Statement {
        const st = self.parseExpression(.lowest);
        if (st) |s| {
            var stmtPtr = self.arena.allocator().create(ast.Statement) catch {
                return null;
            };
            stmtPtr.* = .{ .expressionStatement = .{ .expression = s } };
            if (self.peekTokenIs(.semicolon)) {
                self.nextToken();
            }
            return stmtPtr;
        } else {
            return null;
        }
    }

    fn parseExpression(self: *Self, precedence: Precedence) ?*ast.Expression {
        _ = precedence;
        const prefix = self.prefixParseFns.get(self.curToken.id());
        if (prefix) |p| {
            var leftExp = p(self);
            // while (!self.peekTokenIs(.semicolon) and precedence < self.peekPrecedence()) {
            //     const infix = self.infixParseFns.get(self.peekToken);
            //     if (!infix) {
            //         return leftExp;
            //     }
            //     self.nextToken();
            //     leftExp = infix(leftExp);
            // }
            return leftExp;
        } else {
            self.noPrefixParseFnError(self.curToken);
            return null;
        }
    }

    fn parsePrefixExpression(self: *Self) ?*ast.Expression {
        const operator = self.curToken;
        self.nextToken();

        var rightExpr = self.arena.allocator().create(ast.Expression) catch {
            return null;
        };

        rightExpr = self.parseExpression(.prefix) orelse {
            return null;
        };

        const op = std.fmt.allocPrint(self.arena.allocator(), "{s}", .{operator}) catch blk: {
            break :blk "";
        };
        // var rr: ?*ast.Expression = undefined;
        // if (right) |r| {
        //     rr = &r;
        //     return .{ .prefix = .{ .operator = op, .right = rr } };
        // }

        var exp = self.arena.allocator().create(ast.Expression) catch {
            return null;
        };
        exp.* = .{ .prefix = .{ .operator = op, .right = rightExpr } };

        return exp;
    }

    fn parseLetStatement(self: *Self) ?*ast.Statement {
        if (!self.expectPeekIsIdent()) {
            return null;
        }

        var stmt = self.arena.allocator().create(ast.Statement) catch {
            return null;
        };

        const name = parseIdentifier(self.curToken);
        const ident = ast.Identifier.init(name);

        if (!self.expectPeek(.assign)) {
            self.nextToken();
        }

        while (!self.curTokenIs(.semicolon)) {
            self.nextToken();
        }

        stmt.* = .{ .letStatement = .{ .identifier = ident, .expression = .{ .identifier = ident } } };

        return stmt;
    }

    fn parseReturnStatement(self: *Self) ?*ast.Statement {
        var stmt = self.arena.allocator().create(ast.Statement) catch {
            return null;
        };

        self.nextToken();
        while (!self.curTokenIs(.semicolon)) {
            self.nextToken();
        }
        stmt.* = .{ .returnStatement = .{ .expression = .{ .identifier = ast.Identifier.init("return") } } };
        return stmt;
    }

    fn parseIdentifierExpression(self: *Self) ?*ast.Expression {
        var ident = self.arena.allocator().create(ast.Expression) catch {
            return null;
        };
        ident.* = .{ .identifier = .{ .identifier = parseIdentifier(self.curToken) } };
        return ident;
        // return .{ .identifier = ast.Identifier.init(parseIdentifier(self.curToken)) };
    }

    fn parseIdentifier(t: token.Token) []const u8 {
        switch (t) {
            .ident => |i| return i,
            else => return "",
        }
    }

    fn parseIntegerLiteral(self: *Self) ?*ast.Expression {
        var int = self.arena.allocator().create(ast.Expression) catch {
            return null;
        };
        return switch (self.curToken) {
            .int => |i| blk: {
                const integer = std.fmt.parseInt(i64, i, 10) catch cblk: {
                    break :cblk -1;
                };
                int.* = .{ .integer = integer };
                break :blk int;
            },
            else => blk: {
                const msg = std.fmt.allocPrint(self.arena.allocator(), "could not parse {s} as integer", .{self.curToken}) catch unreachable;
                self.errors.append(self.arena.allocator(), msg) catch {
                    std.debug.panic("failed to append error message", .{});
                };
                int.* = .{ .integer = -1 };
                break :blk int;
            },
        };
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
            self.peekError(.{ .ident = "" });
            return false;
        }
    }

    fn expectPeek(self: *Self, t: token.Token) bool {
        if (self.peekTokenIs(t)) {
            self.nextToken();
            return true;
        } else {
            self.peekError(t);
            return false;
        }
    }
};

fn checkParserErrors(errors: []const []const u8) void {
    if (errors.len == 0) {
        return;
    }
    std.log.warn("parser has {d} errors", .{errors.len});
    for (errors) |msg| {
        std.log.warn("parser error: {s}", .{msg});
    }
}

const assert = std.debug.assert;
const test_allocator = std.testing.allocator;
test "test let statement" {
    const input =
        \\let x = 5;
        \\let y =  10;
        \\let foobar = 838383;
    ;

    const l = lexer.Lexer.init(input);
    var p = Parser.init(l, test_allocator);
    defer p.arena.deinit();

    var prog = p.parseProgram();
    checkParserErrors(p.getErrors());

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
        std.log.warn("{s}", .{stmt});
        assert(std.mem.eql(u8, @tagName(stmt.*), @tagName(ast.Statement.letStatement)));
        assert(std.mem.eql(u8, stmt.letStatement.identifier.identifier, tt.expected));
    }
}

test "test return statement" {
    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    var l = lexer.Lexer.init(input);
    var p = Parser.init(l, test_allocator);
    defer p.arena.deinit();

    var prog = p.parseProgram();
    checkParserErrors(p.getErrors());

    assert(prog.statements.items.len == 3);

    const stmt = prog.statements.items[0];
    std.log.warn("{s}", .{stmt});
    assert(std.mem.eql(u8, @tagName(stmt.*), @tagName(ast.Statement.returnStatement)));
}

test "test identifier experssion" {
    const input = "foobar;";

    var l = lexer.Lexer.init(input);
    var p = Parser.init(l, test_allocator);
    defer p.arena.deinit();

    var prog = p.parseProgram();
    checkParserErrors(p.getErrors());

    assert(prog.statements.items.len == 1);

    for (prog.statements.items) |stmt| {
        std.log.warn("{s}", .{stmt});
        assert(std.mem.eql(u8, @tagName(stmt.*), @tagName(ast.Statement.expressionStatement)));
        assert(std.mem.eql(u8, stmt.expressionStatement.expression.identifier.identifier, "foobar"));
    }
}

test "test integer literal experssion" {
    const input = "5;";

    var l = lexer.Lexer.init(input);
    var p = Parser.init(l, test_allocator);
    defer p.arena.deinit();

    var prog = p.parseProgram();
    checkParserErrors(p.getErrors());

    assert(prog.statements.items.len == 1);

    const stmt = prog.statements.items[0];
    std.log.warn("{s}", .{stmt});
    assert(std.mem.eql(u8, @tagName(stmt.*), @tagName(ast.Statement.expressionStatement)));
    assert(stmt.expressionStatement.expression.integer == 5);
}

test "test parsing prefix expressions" {
    const prefixTests = [_]struct {
        input: []const u8,
        operator: []const u8,
        integerValue: i64,
    }{
        .{ .input = "!5;", .operator = "!", .integerValue = 5 },
        // .{ .input = "-15;", .operator = "-", .integerValue = 15 },
    };

    for (prefixTests) |tt| {
        var l = lexer.Lexer.init(tt.input);
        var p = Parser.init(l, test_allocator);
        defer p.arena.deinit();

        var prog = p.parseProgram();
        checkParserErrors(p.getErrors());

        assert(prog.statements.items.len == 1);

        const stmt = prog.statements.items[0];
        // std.log.warn("{s}", .{stmt});
        // std.log.warn("{s}, {s}", .{ @tagName(stmt.expressionStatement.expression.*), @tagName(ast.Expression.prefix) });
        assert(std.mem.eql(u8, @tagName(stmt.*), @tagName(ast.Statement.expressionStatement)));
        assert(std.mem.eql(u8, @tagName(stmt.*.expressionStatement.expression.*), @tagName(ast.Expression.prefix)));
        assert(std.mem.eql(u8, stmt.expressionStatement.expression.prefix.operator, tt.operator));
        const r = stmt.expressionStatement.expression.prefix.right.?;
        assert(r.integer == tt.integerValue);
    }
}
