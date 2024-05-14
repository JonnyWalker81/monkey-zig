const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const object = @import("object.zig");
const code = @import("code.zig");
const compiler = @import("compiler.zig");

const StackSize = 2048;

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
                    var buf: [2]u8 = undefined;
                    const start = ip + 1;
                    // std.log.warn("start: {d}", .{start});
                    // std.log.warn("instructions: {d}", .{self.instructions});
                    @memcpy(&buf, self.instructions[start .. start + 2]);
                    const constIndex = std.mem.readInt(u16, &buf, .big);
                    ip += 2;

                    const obj = self.constants[constIndex];
                    try self.push(obj);
                },
                .OpAdd => {
                    const right = self.pop();
                    const left = self.pop();
                    const result = object.Object{ .integer = left.intValue() + right.intValue() };
                    try self.push(result);
                },
                .OpPop => {
                    _ = self.pop();
                },
            }

            ip += 1;
        }
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
    integer: usize,
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
