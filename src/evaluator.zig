const std = @import("std");
const object = @import("object.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const program = @import("program.zig");
const ast = @import("ast.zig");
const environment = @import("environment.zig");
const ArrayListUnmanaged = std.ArrayListUnmanaged;

// var NULL = .nil;
var TRUE = object.Object{ .boolean = true };
var FALSE = object.Object{ .boolean = false };

pub const Evaluator = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{ .arena = std.heap.ArenaAllocator.init(allocator) };
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }

    pub fn eval(self: *Self, node: ast.Node, env: *environment.Environment) ?*object.Object {
        switch (node) {
            .program => |p| {
                // std.log.warn("evaluating program...", .{});
                return self.eval_program(p, env);
            },
            .expression => |es| {
                return self.eval_expression(es, env);
            },
            .statement => |s| {
                return self.eval_statement(s, env);
            },
        }
    }

    fn eval_program(self: *Self, prog: *program.Program, env: *environment.Environment) ?*object.Object {
        var result: ?*object.Object = undefined;
        for (prog.statements.items) |stmt| {
            result = self.eval_statement(stmt, env);

            if (result) |r| {
                switch (r.*) {
                    .returnValue => |rv| {
                        return rv;
                    },
                    .err => |_| {
                        return result;
                    },
                    else => {},
                }
            }
        }
        return result;
    }

    // pub fn eval_program(self: *Self, statement: *ast.Statement) *object.Object {
    //     return self.eval_statement(statement);
    // }

    pub fn eval_statement(self: *Self, statement: *ast.Statement, env: *environment.Environment) ?*object.Object {
        switch (statement.*) {
            .letStatement => |ls| {
                const value = self.eval_expression(ls.expression, env);
                if (is_error(value)) {
                    return value;
                }
                if (value) |v| {
                    // std.log.warn("setting {s} to {s}", .{ ls.identifier.identifier, v });
                    _ = env.set(ls.identifier.identifier, v);
                }
                return value;
            },
            .expressionStatement => |es| {
                return self.eval_expression(es.expression, env);
            },
            .blockStatement => |bs| {
                var result = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                for (bs.statements.items) |stmt| {
                    result = self.eval_statement(stmt, env) orelse return null;
                }
                return result;
            },
            .returnStatement => |rs| {
                var result = self.eval_expression(rs.expression, env);
                if (is_error(result)) {
                    return result;
                }
                var obj = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = .{ .returnValue = result.? };
                return obj;
            },
            else => {
                std.log.warn("unknown statement...{any}", .{statement});
                // var obj: *object.Object = self.allocator.create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                return null;
            },
        }
    }

    pub fn eval_expression(self: *Self, expression: *ast.Expression, env: *environment.Environment) ?*object.Object {
        switch (expression.*) {
            .identifier => |i| {
                if (env.get(i.identifier)) |obj| {
                    return obj;
                } else {
                    return self.new_error("identifier not found: {s}", .{i.identifier});
                }
            },
            .integer => |i| {
                var obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = .{ .integer = i };
                return obj;
            },
            .boolean => |b| {
                var obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = if (b) TRUE else FALSE;
                return obj;
            },
            .prefix => |p| {
                var right = self.eval_expression(p.right, env);
                if (is_error(right)) {
                    return right;
                }

                if (right) |r| {
                    return self.eval_prefix_expression(p.operator, r);
                } else {
                    return null;
                }
            },
            .infix => |i| {
                var left = self.eval_expression(i.left, env);
                if (is_error(left)) {
                    return left;
                }
                var right = self.eval_expression(i.right, env);
                if (is_error(right)) {
                    return right;
                }
                return self.eval_infix_expression(i.operator, left.?, right.?);
            },
            .ifExpression => |_| {
                return self.eval_if_expression(expression, env);
            },
            .functionLiteral => |fl| {
                var obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                std.log.warn("functionLiteral: {any}\n", .{fl.parameters.items[0]});
                obj.* = .{ .function = .{ .parameters = fl.parameters.clone(self.arena.allocator()) catch unreachable, .body = fl.body, .env = env } };
                return obj;
            },
            else => {
                return null;
            },
        }
    }

    fn eval_if_expression(self: *Self, ie: *ast.Expression, env: *environment.Environment) ?*object.Object {
        return switch (ie.*) {
            .ifExpression => |ifExpr| {
                var condition = self.eval_expression(ifExpr.condition, env);
                if (is_error(condition)) {
                    return condition;
                }

                if (condition.?.boolValue()) {
                    return self.eval_block_statement(ifExpr.consequence, env);
                } else if (ifExpr.alternative) |alternative| {
                    return self.eval_block_statement(alternative, env);
                } else {
                    var obj = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                    obj.* = .nil;
                    return obj;
                }
            },
            else => {
                return null;
            },
        };
    }

    fn eval_block_statement(self: *Self, bs: *ast.BlockStatement, env: *environment.Environment) ?*object.Object {
        var result: ?*object.Object = undefined;
        for (bs.statements.items) |stmt| {
            result = self.eval_statement(stmt, env);

            if (result) |r| {
                switch (r.*) {
                    .returnValue, .err => {
                        return result;
                    },
                    else => {},
                }
            }
        }
        return result;
    }

    pub fn eval_infix_expression(self: *Self, operator: []const u8, left: *object.Object, right: *object.Object) ?*object.Object {
        if (!std.mem.eql(u8, left.typeId(), right.typeId())) {
            return self.new_error("type mismatch: {s} {s} {s}", .{ left.typeId(), operator, right.typeId() });
        }
        if (left.* == .integer and right.* == .integer) {
            return self.eval_integer_infix_expression(operator, left, right);
        } else if (std.mem.eql(u8, operator, "==")) {
            return self.eval_boolean_infix_expression(operator, left, right);
        } else if (std.mem.eql(u8, operator, "!=")) {
            return self.eval_boolean_infix_expression(operator, left, right);
        } else {
            return self.new_error("unknown operator: {s} {s} {s}", .{ left.typeId(), operator, right.typeId() });
        }
    }

    pub fn eval_boolean_infix_expression(self: *Self, operator: []const u8, left: *object.Object, right: *object.Object) ?*object.Object {
        var obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
        if (std.mem.eql(u8, operator, "==")) {
            obj.* = .{ .boolean = left.boolValue() == right.boolValue() };
            return obj;
        } else if (std.mem.eql(u8, operator, "!=")) {
            obj.* = .{ .boolean = left.boolValue() != right.boolValue() };
            return obj;
        } else {
            return null;
        }
    }

    pub fn eval_integer_infix_expression(self: *Self, operator: []const u8, left: *object.Object, right: *object.Object) ?*object.Object {
        var obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
        if (std.mem.eql(u8, operator, "+")) {
            obj.* = .{ .integer = left.intValue() + right.intValue() };
            return obj;
        } else if (std.mem.eql(u8, operator, "-")) {
            obj.* = .{ .integer = left.intValue() - right.intValue() };
            return obj;
        } else if (std.mem.eql(u8, operator, "*")) {
            obj.* = .{ .integer = left.intValue() * right.intValue() };
            return obj;
        } else if (std.mem.eql(u8, operator, "/")) {
            obj.* = .{ .integer = @divExact(left.intValue(), right.intValue()) };
            return obj;
        } else if (std.mem.eql(u8, operator, "<")) {
            obj.* = .{ .boolean = left.intValue() < right.intValue() };
            return obj;
        } else if (std.mem.eql(u8, operator, ">")) {
            obj.* = .{ .boolean = left.intValue() > right.intValue() };
            return obj;
        } else if (std.mem.eql(u8, operator, "==")) {
            obj.* = .{ .boolean = left.intValue() == right.intValue() };
            return obj;
        } else if (std.mem.eql(u8, operator, "!=")) {
            obj.* = .{ .boolean = left.intValue() != right.intValue() };
            return obj;
        } else {
            return self.new_error("unknown operator: {s} {s} {s}", .{ left.typeId(), operator, right.typeId() });
        }
    }

    pub fn eval_prefix_expression(self: *Self, operator: []const u8, right: *object.Object) ?*object.Object {
        if (std.mem.eql(u8, operator, "!")) {
            return self.eval_bang_operator(right);
        } else if (std.mem.eql(u8, operator, "-")) {
            return self.eval_minus_prefix_operator(right);
        } else {
            return self.new_error("unknown operator: {s}{s}", .{ operator, right.typeId() });
        }
    }

    pub fn eval_minus_prefix_operator(self: *Self, right: *object.Object) ?*object.Object {
        switch (right.*) {
            .integer => |i| {
                var obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = .{ .integer = -i };
                return obj;
            },
            else => {
                return self.new_error("unknown operator: -{s}", .{right.typeId()});
            },
        }
    }

    pub fn eval_bang_operator(self: *Self, right: *object.Object) ?*object.Object {
        switch (right.*) {
            .boolean => |b| {
                var result = if (b) FALSE else TRUE;
                var obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = result;
                return obj;
            },
            .nil => {
                var obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = .{ .boolean = true };
                return obj;
            },
            else => {
                var obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = .{ .boolean = false };
                return obj;
            },
        }
    }

    pub fn is_error(obj: ?*object.Object) bool {
        if (obj) |o| {
            return o.* == .err;
        }
        return false;
    }

    pub fn new_error(self: *Self, comptime fmt: []const u8, args: anytype) *object.Object {
        var obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
        const msg = std.fmt.allocPrint(self.arena.allocator(), fmt, args) catch unreachable;
        obj.* = .{ .err = msg };
        return obj;
    }
};

const assert = std.debug.assert;
var test_allocator = std.testing.allocator;

fn test_eval(evaluator: *Evaluator, input: []const u8) *object.Object {
    // std.log.warn("input: {s}\n", .{input});
    var l = lexer.Lexer.init(input);
    var p = parser.Parser.init(l, test_allocator);
    defer p.deinit();
    var prog = p.parseProgram();
    var node = .{ .program = &prog };
    // std.log.warn("node: {any}\n", .{node});
    // std.log.warn("evaluator: {any}\n", .{evaluator});
    var env = environment.Environment.init(test_allocator);
    defer env.deinit();
    return evaluator.eval(node, &env) orelse std.debug.panic("failed to evaluate", .{});
}

fn test_integer_object(obj: *object.Object, expected: i64) void {
    switch (obj.*) {
        .integer => |i| {
            // std.log.warn("{d} == {d}\n", .{ i, expected });
            assert(i == expected);
        },
        else => {
            std.log.warn("object is not Integer. got={s}\n", .{obj});
        },
    }
}

fn test_boolean_object(obj: *object.Object, expected: bool) void {
    switch (obj.*) {
        .boolean => |b| {
            assert(b == expected);
        },
        else => {
            std.log.warn("object is not Boolean. got={s}\n", .{obj});
        },
    }
}

test "test eval integer expression" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "5", .expected = 5 },
        .{ .input = "10", .expected = 10 },
        .{ .input = "-5", .expected = -5 },
        .{ .input = "-10", .expected = -10 },
        .{ .input = "5 + 5 + 5 + 5 - 10", .expected = 10 },
        .{ .input = "2 * 2 * 2 * 2 * 2", .expected = 32 },
        .{ .input = "-50 + 100 + -50", .expected = 0 },
        .{ .input = "5 * 2 + 10", .expected = 20 },
        .{ .input = "5 + 2 * 10", .expected = 25 },
        .{ .input = "20 + 2 * -10", .expected = 0 },
        .{ .input = "50 / 2 * 2 + 10", .expected = 60 },
        .{ .input = "2 * (5 + 10)", .expected = 30 },
        .{ .input = "3 * 3 * 3 + 10", .expected = 37 },
        .{ .input = "3 * (3 * 3) + 10", .expected = 37 },
        .{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .expected = 50 },
    };

    // std.log.warn("tests: {d}\n", .{tests.len});

    for (tests) |t| {
        // std.log.warn("input: {s}\n", .{t.input});
        var evaluator = Evaluator.init(test_allocator);
        const o = test_eval(&evaluator, t.input);
        defer evaluator.deinit();
        // std.log.warn("o: {s}\n", .{o});
        // _ = test_eval(t.input);
        test_integer_object(o, t.expected);
    }
}

test "test eval boolean expression" {
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "true", .expected = true },
        .{ .input = "false", .expected = false },
        .{ .input = "1 < 2", .expected = true },
        .{ .input = "1 > 2", .expected = false },
        .{ .input = "1 < 1", .expected = false },
        .{ .input = "1 > 1", .expected = false },
        .{ .input = "1 == 1", .expected = true },
        .{ .input = "1 != 1", .expected = false },
        .{ .input = "1 == 2", .expected = false },
        .{ .input = "1 != 2", .expected = true },
        .{ .input = "true == true", .expected = true },
        .{ .input = "false == false", .expected = true },
        .{ .input = "true == false", .expected = false },
        .{ .input = "true != false", .expected = true },
        .{ .input = "false != true", .expected = true },
        .{ .input = "(1 < 2) == true", .expected = true },
        .{ .input = "(1 < 2) == false", .expected = false },
        .{ .input = "(1 > 2) == true", .expected = false },
        .{ .input = "(1 > 2) == false", .expected = true },
    };

    for (tests) |t| {
        var evaluator = Evaluator.init(test_allocator);
        const o = test_eval(&evaluator, t.input);
        defer evaluator.deinit();
        // std.log.warn("o: {s}\n", .{o});
        // _ = test_eval(t.input);
        test_boolean_object(o, t.expected);
    }
}

test "test bang operator" {
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "!true", .expected = false },
        .{ .input = "!false", .expected = true },
        .{ .input = "!5", .expected = false },
        .{ .input = "!!true", .expected = true },
        .{ .input = "!!false", .expected = false },
        .{ .input = "!!5", .expected = true },
    };

    for (tests) |t| {
        var evaluator = Evaluator.init(test_allocator);
        const o = test_eval(&evaluator, t.input);
        defer evaluator.deinit();
        test_boolean_object(o, t.expected);
    }
}

const val = union(enum) {
    integer: i64,
    nil,
};

test "test if else expressions" {
    const tests = [_]struct {
        input: []const u8,
        expected: val,
    }{
        .{ .input = "if (true) { 10 }", .expected = .{ .integer = 10 } },
        .{ .input = "if (false) { 10 }", .expected = .nil },
        .{ .input = "if (1) { 10 }", .expected = .{ .integer = 10 } },
        .{ .input = "if (1 < 2) { 10 }", .expected = .{ .integer = 10 } },
        .{ .input = "if (1 > 2) { 10 }", .expected = .nil },
        .{ .input = "if (1 > 2) { 10 } else { 20 }", .expected = .{ .integer = 20 } },
        .{ .input = "if (1 < 2) { 10 } else { 20 }", .expected = .{ .integer = 10 } },
    };

    for (tests) |t| {
        var evaluator = Evaluator.init(test_allocator);
        const o = test_eval(&evaluator, t.input);
        defer evaluator.deinit();
        switch (t.expected) {
            .integer => {
                test_integer_object(o, t.expected.integer);
            },
            .nil => {
                assert(o.* == .nil);
            },
        }
    }
}

test "test return statements" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "return 10;", .expected = 10 },
        .{ .input = "return 10; 9;", .expected = 10 },
        .{ .input = "return 2 * 5; 9;", .expected = 10 },
        .{ .input = "9; return 2 * 5; 9;", .expected = 10 },
        .{ .input = "if (10 > 1) { if (10 > 1) { return 10; } return 1; }", .expected = 10 },
    };

    for (tests) |t| {
        var evaluator = Evaluator.init(test_allocator);
        const o = test_eval(&evaluator, t.input);
        defer evaluator.deinit();
        test_integer_object(o, t.expected);
    }
}

test "test error handling" {
    const tests = [_]struct {
        input: []const u8,
        expected: []const u8,
    }{
        .{ .input = "5 + true;", .expected = "type mismatch: INTEGER + BOOLEAN" },
        .{ .input = "5 + true; 5;", .expected = "type mismatch: INTEGER + BOOLEAN" },
        .{ .input = "-true", .expected = "unknown operator: -BOOLEAN" },
        .{ .input = "true + false;", .expected = "unknown operator: BOOLEAN + BOOLEAN" },
        .{ .input = "5; true + false; 5", .expected = "unknown operator: BOOLEAN + BOOLEAN" },
        .{ .input = "if (10 > 1) { true + false; }", .expected = "unknown operator: BOOLEAN + BOOLEAN" },
        .{ .input = "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }", .expected = "unknown operator: BOOLEAN + BOOLEAN" },
        .{ .input = "foobar", .expected = "identifier not found: foobar" },
    };

    for (tests) |t| {
        var evaluator = Evaluator.init(test_allocator);
        const o = test_eval(&evaluator, t.input);
        defer evaluator.deinit();
        // std.log.warn("actual: {s}\n", .{o});
        // std.log.warn("expected: {s}\n", .{t.expected});
        switch (o.*) {
            .err => |e| {
                // std.log.warn("o: {s}\n", .{o});
                assert(std.mem.eql(u8, e, t.expected));
            },
            else => {
                std.debug.panic("no error object returned", .{});
            },
        }
    }
}

test "test let statement" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "let a = 5; a;", .expected = 5 },
        .{ .input = "let a = 5 * 5; a;", .expected = 25 },
        .{ .input = "let a = 5; let b = a; b;", .expected = 5 },
        .{ .input = "let a = 5; let b = a; let c = a + b + 5; c;", .expected = 15 },
    };

    for (tests) |t| {
        var evaluator = Evaluator.init(test_allocator);
        const o = test_eval(&evaluator, t.input);
        defer evaluator.deinit();
        test_integer_object(o, t.expected);
    }
}

test "test function object" {
    const input = "fn(x) { x + 2; }";
    var evaluator = Evaluator.init(test_allocator);
    defer evaluator.deinit();
    const o = test_eval(&evaluator, input);
    // std.log.warn("o: {s}\n", .{o});
    switch (o.*) {
        .function => |f| {
            assert(f.parameters.items.len == 1);
            const param = f.parameters.items[0];
            std.log.warn("f.parameters.items[0].identifier: {any}\n", .{param});
            // std.log.warn("f.parameters.items[0].identifier: {any}\n", .{f.parameters.items});
            assert(std.mem.eql(u8, param.identifier, "x"));
            // _ = std.fmt.allocPrint(test_allocator, "{s}", .{f.body}) catch unreachable;
            // std.log.warn("body: {s}\n", .{body});
            // assert(std.mem.eql(u8, body, "(x + 2)"));
        },
        else => {
            std.debug.panic("object is not Function. got={s}", .{o});
        },
    }
}
