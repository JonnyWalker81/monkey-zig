const std = @import("std");
const object = @import("object.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

var NULL = object.Object{.nil};
var TRUE = object.Object{ .boolean = true };
var FALSE = object.Object{ .boolean = false };

pub const Evaluator = struct {
    const Self = @This();

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{ .allocator = allocator };
    }

    pub fn eval(self: *Self, node: ast.Node) ?*object.Object {
        // std.log.warn("here...", .{});
        switch (node) {
            .program => |p| {
                // std.log.warn("evaluating program...", .{});
                var result: ?*object.Object = null;
                for (p.statements.items) |statement| {
                    result = self.eval_statement(statement);
                }
                return result;
            },
            .expression => |es| {
                return self.eval_expression(es);
            },
            .statement => |s| {
                return self.eval_statement(s);
            },
        }
    }

    // pub fn eval_program(self: *Self, statement: *ast.Statement) *object.Object {
    //     return self.eval_statement(statement);
    // }

    pub fn eval_statement(self: *Self, statement: *ast.Statement) ?*object.Object {
        switch (statement.*) {
            .letStatement => |ls| {
                // std.log.warn("let statement...", .{});
                const value = self.eval_expression(ls.expression);
                // ls.env.set(ls.name.value, value);
                return value;
            },
            .expressionStatement => |es| {
                return self.eval_expression(es.expression);
            },
            else => {
                // var obj: *object.Object = self.allocator.create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                return null;
            },
        }
    }

    pub fn eval_expression(self: *Self, expression: *ast.Expression) ?*object.Object {
        switch (expression.*) {
            .integer => |i| {
                var obj: *object.Object = self.allocator.create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = .{ .integer = i };
                return obj;
            },
            .boolean => |b| {
                var obj: *object.Object = self.allocator.create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = if (b) TRUE else FALSE;
                return obj;
            },
            else => {
                return null;
            },
        }
    }
};

const assert = std.debug.assert;
var test_allocator = std.testing.allocator;
test "test eval integer expression" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "5", .expected = 5 },
        .{ .input = "10", .expected = 10 },
    };

    // std.log.warn("tests: {d}\n", .{tests.len});

    for (tests) |t| {
        // std.log.warn("input: {s}\n", .{t.input});
        const o = test_eval(t.input);
        defer test_allocator.destroy(o);
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
    };

    for (tests) |t| {
        const o = test_eval(t.input);
        defer test_allocator.destroy(o);
        // std.log.warn("o: {s}\n", .{o});
        // _ = test_eval(t.input);
        test_boolean_object(o, t.expected);
    }
}

fn test_eval(input: []const u8) *object.Object {
    // std.log.warn("input: {s}\n", .{input});
    var l = lexer.Lexer.init(input);
    var p = parser.Parser.init(l, test_allocator);
    defer p.deinit();
    var program = p.parseProgram();
    var node = .{ .program = &program };
    // std.log.warn("node: {any}\n", .{node});
    var evaluator = Evaluator.init(test_allocator);
    // std.log.warn("evaluator: {any}\n", .{evaluator});
    return evaluator.eval(node) orelse std.debug.panic("failed to evaluate", .{});
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
