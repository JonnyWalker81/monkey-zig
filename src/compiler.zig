const std = @import("std");
const code = @import("code.zig");
const object = @import("object.zig");
const ast = @import("ast.zig");

const Compiler = struct {
    const Self = @This();
    instructions: code.Instructions,
    constants: []const object.Object,

    pub fn init() Compiler {
        return Compiler{
            .instructions = &[_]code.Instruction{},
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
        instructions: []const code.Instructions,
        constants: []const object.Object,
    };
};

const val = union(enum) { int: usize };

const CompilerTestCase = struct {
    input: []const u8,
    expectedConstants: []const val,
    expectedInstructions: []code.Instructions,
};

const test_allocator = std.testing.allocator;
const assert = std.debug.assert;
test "test integer arithmetic" {
    const tests = &[_]CompilerTestCase{
        CompilerTestCase{
            .input = "1 + 2",
            .expectedConstants = &[_]val{ .{ .int = 1 }, .{ .int = 2 } },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, @intFromEnum(code.Constants.OpConstant), &[_]i32{0}),
                code.make(test_allocator, @intFromEnum(code.Constants.OpConstant), &[_]i32{1}),
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

        try testInstructions(tt.expectedInstructions, bytecode.instructions);
        try testConstants(tt.expectedConstants, bytecode.constants);
    }
}

fn parse(input: []const u8) ast.Node {
    const l = ast.Lexer.init(input);
    const p = ast.Parser.init(l);
    const prog = p.parseProgram();

    return .{ .Program = prog };
}

fn testInstructions(expected: []code.Instructions, actual: code.Instructions) !void {
    const concatted = concatInstructions(expected);
    assert(concatted.len == actual.len);

    for (concatted, 0..) |ins, i| {
        const act = actual[i];
        assert(ins == act);
    }

    return null;
}

fn testConstants(expected: []const val, actual: []const object.Object) !void {
    assert(expected.len == actual.len);

    for (expected, actual) |exp, act| {
        switch (exp) {
            .int => |i| {
                testIntegerObject(i, act);
            },
        }
    }

    return null;
}

fn testIntegerObject(expected: usize, actual: object.Object) !void {
    const i = actual.IntValue();
    assert(i == expected);
    return null;
}

fn concatInstructions(slices: code.Instructions) code.Instructions {
    var out = [_]u8{};

    for (slices) |slice| {
        out = out ++ slice;
    }

    return out;
}
