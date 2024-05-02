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

const prefixParserFn = *const fn (*Parser) *ast.Expression;
const infixParserFn = *const fn (*Parser, *ast.Expression) *ast.Expression;

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

        var trueToken: token.Token = .true_token;
        p.registerPrefix(trueToken.id(), parseBooleanExpression) catch {
            std.debug.panic("failed to register prefix", .{});
        };

        var falseToken: token.Token = .false_token;
        p.registerPrefix(falseToken.id(), parseBooleanExpression) catch {
            std.debug.panic("failed to register prefix", .{});
        };

        var lparenToken: token.Token = .lparen;
        p.registerPrefix(lparenToken.id(), parseGroupedExpression) catch {
            std.debug.panic("failed to register prefix", .{});
        };

        var plusToken: token.Token = .plus;
        p.registerInfix(plusToken.id(), parseInfixExpression) catch {
            std.debug.panic("failed to register prefix", .{});
        };

        p.registerInfix(minusToken.id(), parseInfixExpression) catch {
            std.debug.panic("failed to register prefix", .{});
        };

        var slashToken: token.Token = .slash;
        p.registerInfix(slashToken.id(), parseInfixExpression) catch {
            std.debug.panic("failed to register prefix", .{});
        };

        var asteriskToken: token.Token = .asterisk;
        p.registerInfix(asteriskToken.id(), parseInfixExpression) catch {
            std.debug.panic("failed to register prefix", .{});
        };

        var eqToken: token.Token = .eq;
        p.registerInfix(eqToken.id(), parseInfixExpression) catch {
            std.debug.panic("failed to register prefix", .{});
        };

        var notEqToken: token.Token = .not_eq;
        p.registerInfix(notEqToken.id(), parseInfixExpression) catch {
            std.debug.panic("failed to register prefix", .{});
        };

        var ltToken: token.Token = .lt;
        p.registerInfix(ltToken.id(), parseInfixExpression) catch {
            std.debug.panic("failed to register prefix", .{});
        };

        var gtToken: token.Token = .gt;
        p.registerInfix(gtToken.id(), parseInfixExpression) catch {
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

    fn precedenceMapping(t: token.Token) Precedence {
        return switch (t) {
            .eq, .not_eq => .equals,
            .lt, .gt => .lessGreater,
            .plus, .minus => .sum,
            .slash, .asterisk => .product,
            .lparen => .call,
            else => .lowest,
        };
    }

    fn peekPrecedence(self: *Self) Precedence {
        return precedenceMapping(self.peekToken);
    }

    fn curPrecendence(self: *Self) Precedence {
        return precedenceMapping(self.curToken);
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
            prog.statements.append(self.arena.allocator(), stmt) catch {
                std.debug.panic("failed to append statement", .{});
            };
            self.nextToken();
        }

        return prog;
    }

    fn parseStatement(self: *Self) *ast.Statement {
        switch (self.curToken) {
            .let => return self.parseLetStatement(),
            .return_token => return self.parseReturnStatement(),
            else => return parseExpressionStatement(self),
        }
    }

    fn parseExpressionStatement(self: *Self) *ast.Statement {
        var stmtPtr = self.arena.allocator().create(ast.Statement) catch {
            std.debug.panic("failed to create statement", .{});
        };
        const st = self.parseExpression(.lowest);
        if (st) |s| {
            stmtPtr.* = .{ .expressionStatement = .{ .expression = s } };
            if (self.peekTokenIs(.semicolon)) {
                self.nextToken();
            }
        }

        return stmtPtr;
    }

    fn parseExpression(self: *Self, precedence: Precedence) ?*ast.Expression {
        const prefix = self.prefixParseFns.get(self.curToken.id());
        if (prefix) |p| {
            var leftExp = p(self);
            while (!self.peekTokenIs(.semicolon) and @intFromEnum(precedence) < @intFromEnum(self.peekPrecedence())) {
                const infix = self.infixParseFns.get(self.peekToken.id());
                if (infix) |infixFn| {
                    self.nextToken();
                    leftExp = infixFn(self, leftExp);
                } else {
                    return leftExp;
                }
            }
            return leftExp;
        } else {
            self.noPrefixParseFnError(self.curToken);
            return null;
        }
    }

    fn parseInfixExpression(self: *Self, left: *ast.Expression) *ast.Expression {
        const operator = self.curToken;
        const precedence = self.curPrecendence();

        self.nextToken();

        var right = self.arena.allocator().create(ast.Expression) catch {
            std.debug.panic("failed to create expression", .{});
        };
        right = self.parseExpression(precedence) orelse {
            return right;
        };

        var exp = self.arena.allocator().create(ast.Expression) catch {
            std.debug.panic("failed to create expression", .{});
        };
        const op = std.fmt.allocPrint(self.arena.allocator(), "{s}", .{operator}) catch blk: {
            break :blk "";
        };

        exp.* = .{ .infix = .{ .left = left, .operator = op, .right = right } };

        return exp;
    }

    fn parsePrefixExpression(self: *Self) *ast.Expression {
        const operator = self.curToken;
        self.nextToken();

        var rightExpr = self.arena.allocator().create(ast.Expression) catch {
            std.debug.panic("failed to create expression", .{});
        };

        rightExpr = self.parseExpression(.prefix) orelse {
            return rightExpr;
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
            std.debug.panic("failed to create expression", .{});
        };
        exp.* = .{ .prefix = .{ .operator = op, .right = rightExpr } };

        return exp;
    }

    fn parseBooleanExpression(self: *Self) *ast.Expression {
        var exp = self.arena.allocator().create(ast.Expression) catch {
            std.debug.panic("failed to create expression", .{});
        };
        exp.* = .{ .boolean = self.curToken == .true_token };
        return exp;
    }

    fn parseGroupedExpression(self: *Self) *ast.Expression {
        var ee = self.arena.allocator().create(ast.Expression) catch {
            std.debug.panic("failed to create expression", .{});
        };

        self.nextToken();
        var exp = self.parseExpression(.lowest);
        if (exp) |e| {
            if (!self.expectPeek(.rparen)) {
                return ee;
            }

            return e;
        }

        return ee;
    }

    fn parseLetStatement(self: *Self) *ast.Statement {
        var stmt = self.arena.allocator().create(ast.Statement) catch {
            std.debug.panic("failed to create statement", .{});
        };

        if (!self.expectPeekIsIdent()) {
            return stmt;
        }

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

    fn parseReturnStatement(self: *Self) *ast.Statement {
        var stmt = self.arena.allocator().create(ast.Statement) catch {
            std.debug.panic("failed to create statement", .{});
        };

        self.nextToken();
        while (!self.curTokenIs(.semicolon)) {
            self.nextToken();
        }
        stmt.* = .{ .returnStatement = .{ .expression = .{ .identifier = ast.Identifier.init("return") } } };
        return stmt;
    }

    fn parseIdentifierExpression(self: *Self) *ast.Expression {
        var ident = self.arena.allocator().create(ast.Expression) catch {
            std.debug.panic("failed to create expression", .{});
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

    fn parseIntegerLiteral(self: *Self) *ast.Expression {
        var int = self.arena.allocator().create(ast.Expression) catch {
            std.debug.panic("failed to create expression", .{});
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
        // std.log.warn("{s}", .{stmt});
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
    // std.log.warn("{s}", .{stmt});
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
        // std.log.warn("{s}", .{stmt});
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
    // std.log.warn("{s}", .{stmt});
    assert(std.mem.eql(u8, @tagName(stmt.*), @tagName(ast.Statement.expressionStatement)));
    assert(stmt.expressionStatement.expression.integer == 5);
}

const val = union(enum) {
    integer: i64,
    boolean: bool,
};

test "test parsing prefix expressions" {
    const prefixTests = [_]struct {
        input: []const u8,
        operator: []const u8,
        integerValue: val,
    }{
        .{ .input = "!5;", .operator = "!", .integerValue = .{ .integer = 5 } },
        .{ .input = "-15;", .operator = "-", .integerValue = .{ .integer = 15 } },
        .{ .input = "!true;", .operator = "!", .integerValue = .{ .boolean = true } },
        .{ .input = "!false;", .operator = "!", .integerValue = .{ .boolean = false } },
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
        const r = stmt.expressionStatement.expression.prefix.right;
        switch (tt.integerValue) {
            .integer => |i| assert(r.integer == i),
            .boolean => |b| assert(r.boolean == b),
        }
    }
}

test "test parsing infix expressions" {
    const infixTests = [_]struct {
        input: []const u8,
        leftValue: val,
        operator: []const u8,
        rightValue: val,
    }{
        .{ .input = "5 + 5;", .leftValue = .{ .integer = 5 }, .operator = "+", .rightValue = .{ .integer = 5 } },
        .{ .input = "5 - 5;", .leftValue = .{ .integer = 5 }, .operator = "-", .rightValue = .{ .integer = 5 } },
        .{ .input = "5 * 5;", .leftValue = .{ .integer = 5 }, .operator = "*", .rightValue = .{ .integer = 5 } },
        .{ .input = "5 / 5;", .leftValue = .{ .integer = 5 }, .operator = "/", .rightValue = .{ .integer = 5 } },
        .{ .input = "5 > 5;", .leftValue = .{ .integer = 5 }, .operator = ">", .rightValue = .{ .integer = 5 } },
        .{ .input = "5 < 5;", .leftValue = .{ .integer = 5 }, .operator = "<", .rightValue = .{ .integer = 5 } },
        .{ .input = "5 == 5;", .leftValue = .{ .integer = 5 }, .operator = "==", .rightValue = .{ .integer = 5 } },
        .{ .input = "5 != 5;", .leftValue = .{ .integer = 5 }, .operator = "!=", .rightValue = .{ .integer = 5 } },
        .{ .input = "true == true;", .leftValue = .{ .boolean = true }, .operator = "==", .rightValue = .{ .boolean = true } },
        .{ .input = "true != false;", .leftValue = .{ .boolean = true }, .operator = "!=", .rightValue = .{ .boolean = false } },
        .{ .input = "false == false;", .leftValue = .{ .boolean = false }, .operator = "==", .rightValue = .{ .boolean = false } },
    };

    for (infixTests) |tt| {
        var l = lexer.Lexer.init(tt.input);
        var p = Parser.init(l, test_allocator);
        defer p.arena.deinit();

        var prog = p.parseProgram();
        checkParserErrors(p.getErrors());

        assert(prog.statements.items.len == 1);

        const stmt = prog.statements.items[0];
        // std.log.warn("{s}", .{stmt});
        // std.log.warn("{s}, {s}", .{ @tagName(stmt.expressionStatement.expression.*), @tagName(ast.Expression.infix) });
        assert(std.mem.eql(u8, @tagName(stmt.*), @tagName(ast.Statement.expressionStatement)));
        assert(std.mem.eql(u8, @tagName(stmt.*.expressionStatement.expression.*), @tagName(ast.Expression.infix)));
        assert(std.mem.eql(u8, stmt.expressionStatement.expression.infix.operator, tt.operator));

        switch (tt.leftValue) {
            .integer => |i| assert(stmt.expressionStatement.expression.infix.left.integer == i),
            .boolean => |b| assert(stmt.expressionStatement.expression.infix.left.boolean == b),
        }

        switch (tt.rightValue) {
            .integer => |i| assert(stmt.expressionStatement.expression.infix.right.integer == i),
            .boolean => |b| assert(stmt.expressionStatement.expression.infix.right.boolean == b),
        }
    }
}

test "test operator precedence parsing" {
    const tests = [_]struct {
        input: []const u8,
        expected: []const u8,
    }{
        .{ .input = "-a * b;", .expected = "((-a) * b)" },
        .{ .input = "!-a;", .expected = "(!(-a))" },
        .{ .input = "a + b + c;", .expected = "((a + b) + c)" },
        .{ .input = "a + b - c;", .expected = "((a + b) - c)" },
        .{ .input = "a * b * c;", .expected = "((a * b) * c)" },
        .{ .input = "a * b / c;", .expected = "((a * b) / c)" },
        .{ .input = "a + b / c;", .expected = "(a + (b / c))" },
        .{ .input = "a + b * c + d / e - f;", .expected = "(((a + (b * c)) + (d / e)) - f)" },
        .{ .input = "3 + 4; -5 * 5;", .expected = "(3 + 4)((-5) * 5)" },
        .{ .input = "5 > 4 == 3 < 4;", .expected = "((5 > 4) == (3 < 4))" },
        .{ .input = "5 < 4 != 3 > 4;", .expected = "((5 < 4) != (3 > 4))" },
        .{ .input = "3 + 4 * 5 == 3 * 1 + 4 * 5;", .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },
        .{ .input = "true;", .expected = "true" },
        .{ .input = "false;", .expected = "false" },
        .{ .input = "3 > 5 == false;", .expected = "((3 > 5) == false)" },
        .{ .input = "3 < 5 == true;", .expected = "((3 < 5) == true)" },
        .{ .input = "1 + (2 + 3) + 4;", .expected = "((1 + (2 + 3)) + 4)" },
        .{ .input = "(5 + 5) * 2;", .expected = "((5 + 5) * 2)" },
        .{ .input = "2 / (5 + 5);", .expected = "(2 / (5 + 5))" },
        .{ .input = "-(5 + 5);", .expected = "(-(5 + 5))" },
        .{ .input = "!(true == true);", .expected = "(!(true == true))" },
    };

    for (tests) |tt| {
        var l = lexer.Lexer.init(tt.input);
        var p = Parser.init(l, test_allocator);
        defer p.arena.deinit();

        var prog = p.parseProgram();
        checkParserErrors(p.getErrors());

        const actual = try std.fmt.allocPrint(test_allocator, "{s}", .{prog});
        defer test_allocator.free(actual);
        assert(std.mem.eql(u8, actual, tt.expected));
    }
}

test "test boolean expression" {
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "true;", .expected = true },
        .{ .input = "false;", .expected = false },
        // .{ .input = "3 > 5;", .expected = "(3 > 5)" },
        // .{ .input = "3 < 5;", .expected = "(3 < 5)" },
        // .{ .input = "3 == 5;", .expected = "(3 == 5)" },
        // .{ .input = "3 != 5;", .expected = "(3 != 5)" },
        // .{ .input = "true == true;", .expected = "(true == true)" },
        // .{ .input = "true != false;", .expected = "(true != false)" },
        // .{ .input = "false == false;", .expected = "(false == false)" },
    };

    for (tests) |tt| {
        var l = lexer.Lexer.init(tt.input);
        var p = Parser.init(l, test_allocator);
        defer p.arena.deinit();

        var prog = p.parseProgram();
        checkParserErrors(p.getErrors());

        assert(prog.statements.items.len == 1);

        const stmt = prog.statements.items[0];
        // std.log.warn("{s}", .{stmt});
        assert(std.mem.eql(u8, @tagName(stmt.*), @tagName(ast.Statement.expressionStatement)));
        assert(std.mem.eql(u8, @tagName(stmt.*.expressionStatement.expression.*), @tagName(ast.Expression.boolean)));
        assert(stmt.expressionStatement.expression.boolean == tt.expected);
    }
}
