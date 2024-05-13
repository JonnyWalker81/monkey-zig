const std = @import("std");
const code = @import("code.zig");
const object = @import("object.zig");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

const Compiler = struct {
    const Self = @This();
    instructions: code.Instructions,
    constants: []const object.Object,

    pub fn init() Compiler {
        return Compiler{
            .instructions = &[_]u8{},
            .constants = &[_]object.Object{},
        };
    }

    pub fn compile(self: Self, node: ast.Node) !void {
        _ = self;
        _ = node;
    }

    pub fn bytecode(self: Self) Bytecode {
        return Bytecode{
            .instructions = self.instructions,
            .constants = self.constants,
        };
    }

    const Bytecode = struct {
        instructions: code.Instructions,
        constants: []const object.Object,
    };
};

const val = union(enum) { int: usize };

const CompilerTestCase = struct {
    input: []const u8,
    expectedConstants: []const val,
    expectedInstructions: []const code.Instructions,
};

const test_allocator = std.testing.allocator;
const assert = std.debug.assert;
test "test integer arithmetic" {
    std.log.warn("op: {any}", .{@intFromEnum(code.Constants.OpConstant)});

    var definitions = code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);
    const tests = &[_]CompilerTestCase{
        CompilerTestCase{
            .input = "1 + 2",
            .expectedConstants = &[_]val{ .{ .int = 1 }, .{ .int = 2 } },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
            },
        },
        // CompilerTestCase{
        //     .input = "1; 2",
        //     .expectedConstants = val{
        //         .int = 1,
        //     },
        //     .expectedInstructions = &[_]code.Instruction{
        //         code.OpConstant{.index = 0},
        //         code.OpPop{},
        //         code.OpConstant{.index = 1},
        //         code.OpPop{},
        //     },
        // },
    };

    try runCompilerTests(tests);
}

fn runCompilerTests(tests: []const CompilerTestCase) !void {
    for (tests) |tt| {
        const program = parse(tt.input);
        const compiler = Compiler.init();
        try compiler.compile(program);

        const bytecode = compiler.bytecode();

        try testInstructions(test_allocator, tt.expectedInstructions, bytecode.instructions);
        try testConstants(tt.expectedConstants, bytecode.constants);
    }
}

fn parse(input: []const u8) ast.Node {
    var l = lexer.Lexer.init(test_allocator, input);
    defer l.deinit();
    var p = parser.Parser.init(l, test_allocator);
    defer p.deinit();
    var prog = p.parseProgram();

    return .{ .program = &prog };
}

fn testInstructions(allocator: std.mem.Allocator, expected: []const code.Instructions, actual: code.Instructions) !void {
    const concatted = try concatInstructions(allocator, expected);
    assert(concatted.len == actual.len);

    for (concatted, 0..) |ins, i| {
        const act = actual[i];
        assert(ins == act);
    }
}

fn testConstants(expected: []const val, actual: []const object.Object) !void {
    assert(expected.len == actual.len);

    for (expected, actual) |exp, act| {
        switch (exp) {
            .int => |i| {
                try testIntegerObject(i, act);
            },
        }
    }
}

fn testIntegerObject(expected: usize, actual: object.Object) !void {
    const i = actual.intValue();
    assert(i == expected);
}

fn concatInstructions(allocator: std.mem.Allocator, slices: []const code.Instructions) !code.Instructions {
    var totalLength: usize = 0;
    for (slices) |arr| {
        totalLength += arr.len;
    }

    // Allocate the flattened array
    var flattened = try allocator.alloc(u8, totalLength);
    defer allocator.free(flattened);

    var offset: usize = 0;
    for (slices) |arr| {
        for (arr, 0..) |byte, j| {
            flattened[offset + j] = byte;
        }
        offset += arr.len;
    }

    return flattened;
}
