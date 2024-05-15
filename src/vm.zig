const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const object = @import("object.zig");
const code = @import("code.zig");
const compiler = @import("compiler.zig");

const StackSize = 2048;

const True = object.Object{ .boolean = true };
const False = object.Object{ .boolean = false };
const Null: object.Object = .nil;

pub const VM = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    constants: []const object.Object,
    instructions: code.Instructions,

    stack: [StackSize]object.Object,
    sp: usize,

    pub fn init(allocator: std.mem.Allocator, bytecode: compiler.Compiler.Bytecode) Self {
        var stack: [StackSize]object.Object = undefined;
        @memset(&stack, .nil);
        return .{
            .allocator = allocator,
            .constants = bytecode.constants,
            .instructions = bytecode.instructions,
            .stack = stack,
            .sp = 0,
        };
    }

    pub fn stackTop(self: *Self) object.Object {
        if (self.sp == 0) {
            return .nil;
        }

        return self.stack[self.sp - 1];
    }

    pub fn pop(self: *Self) object.Object {
        if (self.sp == 0) {
            return .nil;
        }

        const o = self.stack[self.sp - 1];
        self.sp -= 1;
        return o;
    }

    pub fn run(self: *Self) !void {
        var ip: usize = 0;
        while (ip < self.instructions.len) {
            const op = @as(code.Constants, @enumFromInt(self.instructions[ip]));

            switch (op) {
                .OpConstant => {
                    // var buf: [2]u8 = undefined;
                    // const start = ip + 1;
                    // std.log.warn("start: {d}", .{start});
                    // std.log.warn("instructions: {d}", .{self.instructions});
                    // @memcpy(&buf, self.instructions[start .. start + 2]);
                    // const constIndex = std.mem.readInt(u16, &buf, .big);
                    const constIndex = readUint16(self.instructions, ip + 1);
                    ip += 2;

                    const obj = self.constants[constIndex];
                    try self.push(obj);
                },
                .OpAdd, .OpSub, .OpMul, .OpDiv => {
                    try self.executeBinaryOperation(op);
                },
                .OpTrue => {
                    try self.push(True);
                },
                .OpFalse => {
                    try self.push(False);
                },
                .OpEqual, .OpNotEqual, .OpGreaterThan => {
                    try self.executeComparison(op);
                },
                .OpBang => {
                    try self.executeBangOperator();
                },
                .OpMinus => {
                    try self.executeMinusOperator();
                },
                .OpPop => {
                    _ = self.pop();
                },
                .OpJumpNotTruthy => {
                    const pos = readUint16(self.instructions, ip + 1);
                    ip += 2;

                    const condition = self.pop();
                    if (!condition.boolValue()) {
                        ip = pos - 1;
                    }
                },
                .OpJump => {
                    // var buf: [2]u8 = undefined;
                    // @memcpy(&buf, self.instructions[ip + 1 .. ip + 3]);
                    // const pos = std.mem.readInt(u16, &buf, .big);
                    const pos = readUint16(self.instructions, ip + 1);
                    ip = pos - 1;
                },
                .OpNull => {
                    try self.push(Null);
                },
            }

            ip += 1;
        }
    }

    fn readUint16(s: []const u8, ip: usize) u16 {
        var buf: [2]u8 = undefined;
        @memcpy(&buf, s[ip .. ip + 2]);
        return std.mem.readInt(u16, &buf, .big);
    }

    fn executeMinusOperator(self: *Self) !void {
        const operand = self.pop();

        if (operand != .integer) {
            return std.debug.panic("unsupported type for negation: {s}", .{operand.typeId()});
        }

        const value = operand.intValue();
        try self.push(object.Object{ .integer = -value });
    }

    fn executeBangOperator(self: *Self) !void {
        const operand = self.pop();

        switch (operand) {
            .boolean => {
                switch (operand.boolValue()) {
                    true => {
                        try self.push(False);
                    },
                    false => {
                        try self.push(True);
                    },
                }
            },
            .nil => {
                try self.push(True);
            },
            else => {
                try self.push(False);
            },
        }
    }

    fn executeComparison(self: *Self, op: code.Constants) !void {
        const right = self.pop();
        const left = self.pop();

        if (left == .integer and right == .integer) {
            return try self.executeIntegerComparison(op, left, right);
        }

        switch (op) {
            .OpEqual => {
                try self.push(if (left.boolValue() == right.boolValue()) True else False);
            },
            .OpNotEqual => {
                try self.push(if (left.boolValue() != right.boolValue()) True else False);
            },
            else => {
                return std.debug.panic("unsupported operator: {any} {s} {s}", .{ op, right.typeId(), left.typeId() });
            },
        }

        return;
    }

    fn executeIntegerComparison(self: *Self, op: code.Constants, left: object.Object, right: object.Object) !void {
        const leftVal = left.intValue();
        const rightVal = right.intValue();

        switch (op) {
            .OpEqual => {
                try self.push(if (leftVal == rightVal) True else False);
            },
            .OpNotEqual => {
                try self.push(if (leftVal != rightVal) True else False);
            },
            .OpGreaterThan => {
                try self.push(if (leftVal > rightVal) True else False);
            },
            else => {
                return std.debug.panic("unknown integer operator: {any}", .{op});
            },
        }
    }

    fn executeBinaryOperation(self: *Self, op: code.Constants) !void {
        const right = self.pop();
        const left = self.pop();

        if (left == .integer and right == .integer) {
            return try self.executeBinaryIntegerOperation(op, left, right);
        }

        return std.debug.panic("unsupported types for binary operation: {s} {s}", .{ right.typeId(), left.typeId() });
    }

    fn executeBinaryIntegerOperation(self: *Self, op: code.Constants, left: object.Object, right: object.Object) !void {
        const leftVal = left.intValue();
        const rightVal = right.intValue();

        var result: object.Object = undefined;
        switch (op) {
            .OpAdd => {
                result = object.Object{ .integer = leftVal + rightVal };
            },
            .OpSub => {
                result = object.Object{ .integer = leftVal - rightVal };
            },
            .OpMul => {
                result = object.Object{ .integer = leftVal * rightVal };
            },
            .OpDiv => {
                result = object.Object{ .integer = @divExact(leftVal, rightVal) };
            },
            else => {
                return std.debug.panic("unknown integer operator: {any}", .{op});
            },
        }

        try self.push(result);
    }

    pub fn lastPoppedStackElem(self: *Self) object.Object {
        return self.stack[self.sp];
    }

    pub fn push(self: *Self, obj: object.Object) !void {
        if (self.sp >= StackSize) {
            return std.debug.panic("stack overflow", .{});
        }

        self.stack[self.sp] = obj;
        self.sp += 1;
    }
};

const assert = std.debug.assert;
const test_allocator = std.testing.allocator;

const ExpectedValue = union(enum) {
    integer: i32,
    boolean: bool,
    nil,
};

const vmTestCase = struct {
    input: []const u8,
    expected: ExpectedValue,
};

test "test integer arithmetic" {
    const tests = &[_]vmTestCase{
        .{ .input = "1", .expected = .{ .integer = 1 } },
        .{ .input = "2", .expected = .{ .integer = 2 } },
        .{ .input = "1 + 2", .expected = .{ .integer = 3 } },
        .{ .input = "1 - 2", .expected = .{ .integer = -1 } },
        .{ .input = "1 * 2", .expected = .{ .integer = 2 } },
        .{ .input = "4 / 2", .expected = .{ .integer = 2 } },
        .{ .input = "50 / 2 * 2 + 10 - 5", .expected = .{ .integer = 55 } },
        .{ .input = "5 + 5 + 5 + 5 - 10", .expected = .{ .integer = 10 } },
        .{ .input = "2 * 2 * 2 * 2 * 2", .expected = .{ .integer = 32 } },
        .{ .input = "5 * 2 + 10", .expected = .{ .integer = 20 } },
        .{ .input = "5 + 2 * 10", .expected = .{ .integer = 25 } },
        .{ .input = "5 * (2 + 10)", .expected = .{ .integer = 60 } },
        .{ .input = "-5", .expected = .{ .integer = -5 } },
        .{ .input = "-10", .expected = .{ .integer = -10 } },
        .{ .input = "-50 + 100 + -50", .expected = .{ .integer = 0 } },
        .{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .expected = .{ .integer = 50 } },
    };

    try runTests(tests);
}

test "test boolean expressions" {
    const tests = &[_]vmTestCase{
        .{ .input = "true", .expected = .{ .boolean = true } },
        .{ .input = "false", .expected = .{ .boolean = false } },
        .{ .input = "1 < 2", .expected = .{ .boolean = true } },
        .{ .input = "1 > 2", .expected = .{ .boolean = false } },
        .{ .input = "1 < 1", .expected = .{ .boolean = false } },
        .{ .input = "1 > 1", .expected = .{ .boolean = false } },
        .{ .input = "1 == 1", .expected = .{ .boolean = true } },
        .{ .input = "1 != 1", .expected = .{ .boolean = false } },
        .{ .input = "1 == 2", .expected = .{ .boolean = false } },
        .{ .input = "1 != 2", .expected = .{ .boolean = true } },
        .{ .input = "true == true", .expected = .{ .boolean = true } },
        .{ .input = "false == false", .expected = .{ .boolean = true } },
        .{ .input = "true == false", .expected = .{ .boolean = false } },
        .{ .input = "true != false", .expected = .{ .boolean = true } },
        .{ .input = "false != true", .expected = .{ .boolean = true } },
        .{ .input = "(1 < 2) == true", .expected = .{ .boolean = true } },
        .{ .input = "(1 < 2) == false", .expected = .{ .boolean = false } },
        .{ .input = "(1 > 2) == true", .expected = .{ .boolean = false } },
        .{ .input = "(1 > 2) == false", .expected = .{ .boolean = true } },
        .{ .input = "!true", .expected = .{ .boolean = false } },
        .{ .input = "!false", .expected = .{ .boolean = true } },
        .{ .input = "!5", .expected = .{ .boolean = false } },
        .{ .input = "!!true", .expected = .{ .boolean = true } },
        .{ .input = "!!false", .expected = .{ .boolean = false } },
        .{ .input = "!!5", .expected = .{ .boolean = true } },
        .{ .input = "!(if (false) { 5 })", .expected = .{ .boolean = true } },
    };

    try runTests(tests);
}

test "test conditionals" {
    const tests = &[_]vmTestCase{
        .{ .input = "if (true) { 10 }", .expected = .{ .integer = 10 } },
        .{ .input = "if (true) { 10 } else { 20 }", .expected = .{ .integer = 10 } },
        .{ .input = "if (false) { 10 } else { 20 }", .expected = .{ .integer = 20 } },
        .{ .input = "if (1) { 10 }", .expected = .{ .integer = 10 } },
        .{ .input = "if (1 < 2) { 10 }", .expected = .{ .integer = 10 } },
        .{ .input = "if (1 < 2) { 10 } else { 20 }", .expected = .{ .integer = 10 } },
        .{ .input = "if (1 > 2) { 10 } else { 20 }", .expected = .{ .integer = 20 } },
        .{ .input = "if (1 > 2) { 10 }", .expected = .nil },
        .{ .input = "if (false) { 10 }", .expected = .nil },
        .{ .input = "if ((if (false) { 10 })) { 10 } else { 20 }", .expected = .{ .integer = 20 } },
    };

    try runTests(tests);
}

pub fn runTests(tests: []const vmTestCase) !void {
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    for (tests) |tt| {
        // std.log.warn("input(vm.runTests): {c}", .{tt.input});
        var helper = parse(tt.input);
        defer helper.deinit();
        var comp = compiler.Compiler.init(test_allocator, definitions);
        try comp.compile(helper.node);
        defer comp.deinit();

        var vm = VM.init(test_allocator, comp.bytecode());
        try vm.run();
        const stackElem = vm.lastPoppedStackElem();
        testExpectedObject(tt.expected, stackElem);
    }
}

fn testExpectedObject(expected: ExpectedValue, actual: object.Object) void {
    switch (expected) {
        .integer => {
            const i = actual.intValue();
            assert(i == expected.integer);
        },
        .boolean => {
            const b = actual.boolValue();
            assert(b == expected.boolean);
        },
        .nil => {
            assert(actual == .nil);
        },
    }
}

const Helper = struct {
    lexer: lexer.Lexer,
    parser: parser.Parser,
    node: ast.Node,

    fn deinit(self: *Helper) void {
        self.lexer.deinit();
        self.parser.deinit();
    }
};

fn parse(input: []const u8) Helper {
    // std.log.warn("input: {any}", .{input});
    const l = lexer.Lexer.init(test_allocator, input);
    // defer l.deinit();
    var p = parser.Parser.init(l, test_allocator);
    // defer p.deinit();
    const prog = p.parseProgram();

    // std.log.warn("prog: {any}", .{prog});
    const node = .{ .program = prog };

    return .{
        .lexer = l,
        .parser = p,
        .node = node,
    };
}

fn testIntegerObject(expected: usize, actual: object.Object) !void {
    const i = actual.intValue();
    assert(i == expected);
}
