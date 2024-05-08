const std = @import("std");
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;
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
    errors: ArrayList([]u8),
    prefixParseFns: AutoHashMap(usize, prefixParserFn),
    infixParseFns: AutoHashMap(usize, infixParserFn),
    arena: std.heap.ArenaAllocator,

    pub fn init(l: lexer.Lexer, allocator: std.mem.Allocator) Parser {
        var p: Parser = .{
            .lexer = l,
            .curToken = .illegal,
            .peekToken = .illegal,
            .errors = ArrayList([]u8).init(allocator),
            .prefixParseFns = AutoHashMap(usize, prefixParserFn).init(allocator),
            .infixParseFns = AutoHashMap(usize, infixParserFn).init(allocator),
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

        var ifToken: token.Token = .if_token;
        p.registerPrefix(ifToken.id(), parseIfExpression) catch {
            std.debug.panic("failed to register prefix", .{});
        };

        var functionToken: token.Token = .function;
        p.registerPrefix(functionToken.id(), parseFunctionLiteral) catch {
            std.debug.panic("failed to register prefix", .{});
        };

        var stringToken: token.Token = .{ .string = "" };
        p.registerPrefix(stringToken.id(), parseStringLiteral) catch {
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

        p.registerInfix(lparenToken.id(), parseCallExpression) catch {
            std.debug.panic("failed to register prefix", .{});
        };

        p.nextToken();
        p.nextToken();
        return p;
    }

    pub fn deinit(self: *Self) void {
        self.errors.deinit();
        self.prefixParseFns.deinit();
        self.infixParseFns.deinit();
        self.arena.deinit();
    }

    fn registerPrefix(self: *Self, id: usize, f: prefixParserFn) !void {
        try self.prefixParseFns.put(id, f);
    }

    fn registerInfix(self: *Self, id: usize, f: infixParserFn) !void {
        try self.infixParseFns.put(id, f);
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
        self.errors.append(msg) catch {
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
        self.errors.append(msg) catch {
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
            prog.statements.append(stmt) catch {
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

    fn parseStringLiteral(self: *Self) *ast.Expression {
        var exp = self.arena.allocator().create(ast.Expression) catch {
            std.debug.panic("failed to create expression", .{});
        };
        exp.* = .{ .stringLiteral = parseString(self.curToken) };
        return exp;
    }

    fn parseInfixExpression(self: *Self, left: *ast.Expression) *ast.Expression {
        const operator = self.curToken;
        const precedence = self.curPrecendence();

        self.nextToken();

        var right = self.arena.allocator().create(ast.Expression) catch {
            std.debug.panic("failed to create expression", .{});
        };
        right = self.parseExpression(precedence) orelse {
            std.debug.panic("failed to parse expression", .{});
            return right;
        };

        // std.log.warn("left: {s}", .{left});

        var exp = self.arena.allocator().create(ast.Expression) catch {
            std.debug.panic("failed to create expression", .{});
        };
        const op = std.fmt.allocPrint(self.arena.allocator(), "{s}", .{operator}) catch blk: {
            std.debug.panic("failed to format operator: {s}", .{operator});
            break :blk "";
        };
        // std.log.warn("operator: {s}", .{op});
        // std.log.warn("right: {s}", .{right});

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

    fn parseIfExpression(self: *Self) *ast.Expression {
        var exp = self.arena.allocator().create(ast.Expression) catch {
            std.debug.panic("failed to create expression", .{});
        };

        if (!self.expectPeek(.lparen)) {
            return exp;
        }

        self.nextToken();
        var condition = self.parseExpression(.lowest) orelse {
            return exp;
        };

        if (!self.expectPeek(.rparen)) {
            return exp;
        }

        if (!self.expectPeek(.lbrace)) {
            return exp;
        }

        var consequence = self.parseBlockStatement();

        exp.* = .{ .ifExpression = .{ .condition = condition, .consequence = consequence, .alternative = null } };
        if (self.peekTokenIs(.else_token)) {
            self.nextToken();

            if (!self.expectPeek(.lbrace)) {
                return exp;
            }

            exp.*.ifExpression.alternative = self.parseBlockStatement();
        }

        return exp;
    }

    fn parseBlockStatement(self: *Self) *ast.BlockStatement {
        var block = ast.BlockStatement.init(self.arena.allocator());

        self.nextToken();

        while (!self.curTokenIs(.rbrace) and !self.curTokenIs(.eof)) {
            const stmt = self.parseStatement();
            block.statements.append(stmt) catch {
                std.debug.panic("failed to append statement", .{});
            };
            self.nextToken();
        }

        return block;
    }

    fn parseFunctionLiteral(self: *Self) *ast.Expression {
        var exp = self.arena.allocator().create(ast.Expression) catch {
            std.debug.panic("failed to create expression", .{});
        };

        if (!self.expectPeek(.lparen)) {
            return exp;
        }

        var parameters = self.parseFunctionParameters();
        if (!self.expectPeek(.lbrace)) {
            return exp;
        }

        var body = self.parseBlockStatement();

        exp.* = .{ .functionLiteral = .{ .parameters = parameters, .body = body } };

        return exp;
    }

    fn parseCallExpression(self: *Self, function: *ast.Expression) *ast.Expression {
        var exp = self.arena.allocator().create(ast.Expression) catch {
            std.debug.panic("failed to create expression", .{});
        };

        const arguments = self.parseCallArguments();

        exp.* = .{ .callExpression = .{ .function = function, .arguments = arguments } };

        return exp;
    }

    fn parseCallArguments(self: *Self) ArrayList(*ast.Expression) {
        var args = ArrayList(*ast.Expression).init(self.arena.allocator());

        if (self.peekTokenIs(.rparen)) {
            self.nextToken();
            return args;
        }

        self.nextToken();
        var arg = self.parseExpression(.lowest) orelse {
            return args;
        };

        args.append(arg) catch {
            std.debug.panic("failed to append argument", .{});
        };

        while (self.peekTokenIs(.comma)) {
            self.nextToken();
            self.nextToken();
            var a = self.parseExpression(.lowest) orelse {
                return args;
            };
            args.append(a) catch {
                std.debug.panic("failed to append argument", .{});
            };
        }

        if (!self.expectPeek(.rparen)) {
            return args;
        }

        return args;
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
            return stmt;
        }

        self.nextToken();

        const value = self.parseExpression(.lowest) orelse {
            return stmt;
        };

        if (self.peekTokenIs(.semicolon)) {
            self.nextToken();
        }

        stmt.* = .{ .letStatement = .{ .identifier = ident, .expression = value } };

        return stmt;
    }

    fn parseFunctionParameters(self: *Self) ArrayList(*ast.Identifier) {
        var identifiers = ArrayList(*ast.Identifier).init(self.arena.allocator());
        if (self.peekTokenIs(.rparen)) {
            self.nextToken();
            return identifiers;
        }

        self.nextToken();

        var ident = self.arena.allocator().create(ast.Identifier) catch {
            std.debug.panic("failed to create identifier", .{});
        };
        ident.* = .{ .identifier = parseIdentifier(self.curToken) };
        // std.log.warn("identifier: {s}", .{ident.identifier});
        identifiers.append(ident) catch {
            std.debug.panic("failed to append identifier", .{});
        };

        while (self.peekTokenIs(.comma)) {
            self.nextToken();
            self.nextToken();

            var i = self.arena.allocator().create(ast.Identifier) catch {
                std.debug.panic("failed to create identifier", .{});
            };
            i.* = .{ .identifier = parseIdentifier(self.curToken) };
            identifiers.append(i) catch {
                std.debug.panic("failed to append identifier", .{});
            };
        }

        if (!self.expectPeek(.rparen)) {
            return identifiers;
        }

        return identifiers;
    }

    fn parseReturnStatement(self: *Self) *ast.Statement {
        var stmt = self.arena.allocator().create(ast.Statement) catch {
            std.debug.panic("failed to create statement", .{});
        };

        self.nextToken();

        const ret = self.parseExpression(.lowest) orelse {
            return stmt;
        };

        if (self.peekTokenIs(.semicolon)) {
            self.nextToken();
        }

        stmt.* = .{ .returnStatement = .{ .expression = ret } };

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

    fn parseString(t: token.Token) []const u8 {
        switch (t) {
            .string => |s| return s,
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
                self.errors.append(msg) catch {
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
const val = union(enum) {
    integer: i64,
    boolean: bool,
    str: []const u8,
};

test "test let statement" {
    // const input =
    //     \\let x = 5;
    //     \\let y =  10;
    //     \\let foobar = 838383;
    // ;

    const tests = [_]struct {
        input: []const u8,
        expectedIdentifier: []const u8,
        expected: val,
    }{
        .{ .input = "let x = 5;", .expectedIdentifier = "x", .expected = .{ .integer = 5 } },
        .{ .input = "let y = true;", .expectedIdentifier = "y", .expected = .{ .boolean = true } },
        .{ .input = "let foobar = y;", .expectedIdentifier = "foobar", .expected = .{ .str = "y" } },
    };

    for (tests) |tt| {
        var l = lexer.Lexer.init(test_allocator, tt.input);
        defer l.deinit();
        var p = Parser.init(l, test_allocator);
        defer p.deinit();

        var prog = p.parseProgram();
        checkParserErrors(p.getErrors());

        // std.log.warn("{d}", .{prog.statements.items.len});
        assert(prog.statements.items.len == 1);

        const stmt = prog.statements.items[0];
        // std.log.warn("{s}", .{tt.input});
        // std.log.warn("{s}", .{stmt});
        assert(std.mem.eql(u8, @tagName(stmt.*), @tagName(ast.Statement.letStatement)));
        switch (tt.expected) {
            .integer => |i| assert(stmt.letStatement.expression.integer == i),
            .boolean => |b| assert(stmt.letStatement.expression.boolean == b),
            .str => |s| assert(std.mem.eql(u8, stmt.letStatement.expression.identifier.identifier, s)),
        }
    }
}

test "test return statement" {
    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    var l = lexer.Lexer.init(test_allocator, input);
    defer l.deinit();

    var p = Parser.init(l, test_allocator);
    defer p.deinit();

    var prog = p.parseProgram();
    checkParserErrors(p.getErrors());

    assert(prog.statements.items.len == 3);

    const stmt = prog.statements.items[0];
    // std.log.warn("{s}", .{stmt});
    assert(std.mem.eql(u8, @tagName(stmt.*), @tagName(ast.Statement.returnStatement)));
}

test "test identifier experssion" {
    const input = "foobar;";

    var l = lexer.Lexer.init(test_allocator, input);
    defer l.deinit();

    var p = Parser.init(l, test_allocator);
    defer p.deinit();

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

    var l = lexer.Lexer.init(test_allocator, input);
    defer l.deinit();

    var p = Parser.init(l, test_allocator);
    defer p.deinit();

    var prog = p.parseProgram();
    checkParserErrors(p.getErrors());

    assert(prog.statements.items.len == 1);

    const stmt = prog.statements.items[0];
    // std.log.warn("{s}", .{stmt});
    assert(std.mem.eql(u8, @tagName(stmt.*), @tagName(ast.Statement.expressionStatement)));
    assert(stmt.expressionStatement.expression.integer == 5);
}

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
        var l = lexer.Lexer.init(test_allocator, tt.input);
        defer l.deinit();

        var p = Parser.init(l, test_allocator);
        defer p.deinit();

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
            .str => |_| assert(false),
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
        var l = lexer.Lexer.init(test_allocator, tt.input);
        defer l.deinit();

        var p = Parser.init(l, test_allocator);
        defer p.deinit();

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
            .str => |_| assert(false),
        }

        switch (tt.rightValue) {
            .integer => |i| assert(stmt.expressionStatement.expression.infix.right.integer == i),
            .boolean => |b| assert(stmt.expressionStatement.expression.infix.right.boolean == b),
            .str => |_| assert(false),
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
        .{ .input = "a + add(b * c) + d;", .expected = "((a + add((b * c))) + d)" },
        .{ .input = "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));", .expected = "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))" },
        .{ .input = "add(a + b + c * d / f + g);", .expected = "add((((a + b) + ((c * d) / f)) + g))" },
    };

    for (tests) |tt| {
        var l = lexer.Lexer.init(test_allocator, tt.input);
        defer l.deinit();

        var p = Parser.init(l, test_allocator);
        defer p.deinit();

        var prog = p.parseProgram();
        checkParserErrors(p.getErrors());

        const actual = try std.fmt.allocPrint(test_allocator, "{s}", .{prog});
        defer test_allocator.free(actual);
        // std.log.warn("{s}", .{tt.expected});
        // std.log.warn("{s}", .{actual});
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
        var l = lexer.Lexer.init(test_allocator, tt.input);
        defer l.deinit();

        var p = Parser.init(l, test_allocator);
        defer p.deinit();

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

test "test if expression" {
    const input = "if (x < y) { x }";

    var l = lexer.Lexer.init(test_allocator, input);
    defer l.deinit();

    var p = Parser.init(l, test_allocator);
    defer p.deinit();

    var prog = p.parseProgram();
    checkParserErrors(p.getErrors());

    assert(prog.statements.items.len == 1);

    const stmt = prog.statements.items[0];
    // std.log.warn("{s}", .{stmt});
    // std.log.warn("left: {s}", .{stmt.expressionStatement.expression.ifExpression.condition.infix.left.identifier.identifier);
    assert(std.mem.eql(u8, @tagName(stmt.*), @tagName(ast.Statement.expressionStatement)));
    assert(std.mem.eql(u8, @tagName(stmt.*.expressionStatement.expression.*), @tagName(ast.Expression.ifExpression)));
    assert(std.mem.eql(u8, stmt.expressionStatement.expression.ifExpression.condition.infix.left.identifier.identifier, "x"));
    assert(std.mem.eql(u8, stmt.expressionStatement.expression.ifExpression.condition.infix.operator, "<"));
    assert(std.mem.eql(u8, stmt.expressionStatement.expression.ifExpression.condition.infix.right.identifier.identifier, "y"));
    assert(std.mem.eql(u8, stmt.expressionStatement.expression.ifExpression.consequence.statements.items[0].expressionStatement.expression.identifier.identifier, "x"));
}

test "test function literal parsing" {
    const input = "fn(x, y) { x + y; }";

    var l = lexer.Lexer.init(test_allocator, input);
    defer l.deinit();

    var p = Parser.init(l, test_allocator);
    defer p.deinit();

    var prog = p.parseProgram();
    checkParserErrors(p.getErrors());

    assert(prog.statements.items.len == 1);

    const stmt = prog.statements.items[0];
    // std.log.warn("{s}", .{stmt});
    assert(std.mem.eql(u8, @tagName(stmt.*), @tagName(ast.Statement.expressionStatement)));
    assert(std.mem.eql(u8, @tagName(stmt.*.expressionStatement.expression.*), @tagName(ast.Expression.functionLiteral)));
    assert(stmt.expressionStatement.expression.functionLiteral.parameters.items.len == 2);
    assert(std.mem.eql(u8, stmt.expressionStatement.expression.functionLiteral.parameters.items[0].identifier, "x"));
    assert(std.mem.eql(u8, stmt.expressionStatement.expression.functionLiteral.parameters.items[1].identifier, "y"));
    assert(stmt.expressionStatement.expression.functionLiteral.body.statements.items.len == 1);
    assert(std.mem.eql(u8, stmt.expressionStatement.expression.functionLiteral.body.statements.items[0].expressionStatement.expression.infix.left.identifier.identifier, "x"));
    assert(std.mem.eql(u8, stmt.expressionStatement.expression.functionLiteral.body.statements.items[0].expressionStatement.expression.infix.operator, "+"));
    assert(std.mem.eql(u8, stmt.expressionStatement.expression.functionLiteral.body.statements.items[0].expressionStatement.expression.infix.right.identifier.identifier, "y"));
}

fn initArray(comptime T: anytype, comptime items: []const T) !ArrayList(T) {
    var list = ArrayList(T).init(test_allocator);

    // Since we're using std.meta.Any, handle potential memory allocation failures
    inline for (items) |item| {
        try list.append(item);
    }

    return list;
}

test "test function parameter parsing" {
    var tests = [_]struct {
        input: []const u8,
        expected: ArrayList([]const u8),
        // expected: [][]const u8,
    }{
        // .{ .input = "fn() {};", .expected = [_][]const u8{""} },
        // .{ .input = "fn(x) {};", .expected = [_][]const u8{"x"} },
        // .{ .input = "fn(x, y, z) {};", .expected = [_][]const u8{ "x", "y", "z" } },
        .{ .input = "fn() {};", .expected = try initArray([]const u8, &[_][]const u8{}) },
        .{ .input = "fn(x) {};", .expected = try initArray([]const u8, &[_][]const u8{"x"}) },
        .{ .input = "fn(x, y, z) {};", .expected = try initArray([]const u8, &[_][]const u8{ "x", "y", "z" }) },
    };

    for (tests) |tt| {
        var l = lexer.Lexer.init(test_allocator, tt.input);
        defer l.deinit();

        var p = Parser.init(l, test_allocator);
        defer p.deinit();

        var prog = p.parseProgram();
        checkParserErrors(p.getErrors());

        const stmt = prog.statements.items[0];
        // std.log.warn("{s}", .{stmt});
        assert(std.mem.eql(u8, @tagName(stmt.*), @tagName(ast.Statement.expressionStatement)));
        assert(std.mem.eql(u8, @tagName(stmt.*.expressionStatement.expression.*), @tagName(ast.Expression.functionLiteral)));
        assert(stmt.expressionStatement.expression.functionLiteral.parameters.items.len == tt.expected.items.len);
        for (stmt.expressionStatement.expression.functionLiteral.parameters.items, tt.expected.items) |param, expected| {
            assert(std.mem.eql(u8, param.identifier, expected));
        }
    }

    var i: usize = 0;
    while (i < tests.len) {
        tests[i].expected.deinit();
        i += 1;
    }
}

test "test call experssion parsing" {
    const input = "add(1, 2 * 3, 4 + 5);";

    var l = lexer.Lexer.init(test_allocator, input);
    defer l.deinit();

    var p = Parser.init(l, test_allocator);
    defer p.deinit();

    var prog = p.parseProgram();
    checkParserErrors(p.getErrors());

    assert(prog.statements.items.len == 1);

    const stmt = prog.statements.items[0];
    // std.log.warn("{s}", .{stmt});
    assert(std.mem.eql(u8, @tagName(stmt.*), @tagName(ast.Statement.expressionStatement)));
    assert(std.mem.eql(u8, @tagName(stmt.*.expressionStatement.expression.*), @tagName(ast.Expression.callExpression)));
    assert(std.mem.eql(u8, stmt.expressionStatement.expression.callExpression.function.identifier.identifier, "add"));
    assert(stmt.expressionStatement.expression.callExpression.arguments.items.len == 3);
    assert(stmt.expressionStatement.expression.callExpression.arguments.items[0].integer == 1);
    assert(stmt.expressionStatement.expression.callExpression.arguments.items[1].infix.left.integer == 2);
    assert(std.mem.eql(u8, stmt.expressionStatement.expression.callExpression.arguments.items[1].infix.operator, "*"));
    assert(stmt.expressionStatement.expression.callExpression.arguments.items[1].infix.right.integer == 3);
    assert(stmt.expressionStatement.expression.callExpression.arguments.items[2].infix.left.integer == 4);
    assert(std.mem.eql(u8, stmt.expressionStatement.expression.callExpression.arguments.items[2].infix.operator, "+"));
    assert(stmt.expressionStatement.expression.callExpression.arguments.items[2].infix.right.integer == 5);
}

test "test string literal expression" {
    const input = "\"hello world\";";

    var l = lexer.Lexer.init(test_allocator, input);
    defer l.deinit();

    var p = Parser.init(l, test_allocator);
    defer p.deinit();

    var prog = p.parseProgram();
    checkParserErrors(p.getErrors());

    assert(prog.statements.items.len == 1);

    const stmt = prog.statements.items[0];
    // std.log.warn("{s}", .{stmt});
    assert(std.mem.eql(u8, @tagName(stmt.*), @tagName(ast.Statement.expressionStatement)));
    assert(std.mem.eql(u8, @tagName(stmt.*.expressionStatement.expression.*), @tagName(ast.Expression.stringLiteral)));
    assert(std.mem.eql(u8, stmt.expressionStatement.expression.stringLiteral, "hello world"));
}
