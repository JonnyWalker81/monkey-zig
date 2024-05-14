const std = @import("std");
const code = @import("code.zig");
const object = @import("object.zig");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const utils = @import("utils.zig");

const Compiler = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    instructions: std.ArrayList(u8),
    constants: std.ArrayList(object.Object),
    definitions: code.Definitions,

    pub fn init(allocator: std.mem.Allocator, definitions: code.Definitions) Compiler {
        return Compiler{
            .allocator = allocator,
            .instructions = std.ArrayList(u8).init(allocator),
            .constants = std.ArrayList(object.Object).init(allocator),
            .definitions = definitions,
        };
    }

    pub fn deinit(self: *Self) void {
        self.instructions.deinit();
        self.constants.deinit();
    }

    pub fn compile(self: *Self, node: ast.Node) !void {
        std.log.warn("node (compile): {any}", .{node});
        switch (node) {
            .program => |p| {
                for (p.statements.items) |stmt| {
                    try self.compile(.{ .statement = stmt });
                }
            },
            .expression => |e| {
                switch (e.*) {
                    .infix => |ie| {
                        try self.compile(.{ .expression = ie.left });
                        try self.compile(.{ .expression = ie.right });
                    },
                    .integer => |i| {
                        const integer = object.Object{ .integer = i };
                        const c = try self.addConstant(integer);
                        _ = try self.emit(@intFromEnum(code.Constants.OpConstant), &[_]usize{c});
                    },
                    else => {},
                }
            },
            .statement => |s| {
                std.log.warn("{any}", .{node});
                switch (s.*) {
                    .expressionStatement => |es| {
                        try self.compile(.{ .expression = es.expression });
                    },
                    else => {},
                }
            },
        }
    }

    pub fn bytecode(self: Self) Bytecode {
        std.log.warn("instructions (bytecode): {any}", .{self.instructions.items});
        return Bytecode{
            .instructions = self.instructions.items[0..],
            .constants = self.constants.items[0..],
        };
    }

    pub fn emit(self: *Self, op: code.Opcode, operands: []const usize) !usize {
        const ins = code.make(self.allocator, self.definitions, op, operands);
        defer self.allocator.free(ins);
        const pos = try self.addInstruction(ins);
        return pos;
    }

    pub fn addInstruction(self: *Self, ins: []const u8) !usize {
        std.log.warn("ins (addIns): {any}", .{ins});
        const posNewInstruction = self.instructions.items.len;
        try self.instructions.appendSlice(ins);
        return posNewInstruction;
    }

    pub fn addConstant(self: *Self, obj: object.Object) !usize {
        try self.constants.append(obj);
        return self.constants.items.len - 1;
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

    try runCompilerTests(tests, definitions);

    for (tests) |tt| {
        for (tt.expectedInstructions) |ins| {
            test_allocator.free(ins);
        }
    }
}

fn runCompilerTests(tests: []const CompilerTestCase, definitions: code.Definitions) !void {
    for (tests) |tt| {
        var helper = parse(tt.input);
        defer helper.deinit();
        // var l = lexer.Lexer.init(test_allocator, tt.input);
        // defer l.deinit();
        // var p = parser.Parser.init(l, test_allocator);
        // defer p.deinit();
        // var prog = p.parseProgram();

        // return .{ .program = &prog };
        // const node = .{ .program = &prog };

        var compiler = Compiler.init(test_allocator, definitions);
        defer compiler.deinit();
        // try compiler.compile(program);
        try compiler.compile(helper.node);

        const bytecode = compiler.bytecode();

        try testInstructions(test_allocator, tt.expectedInstructions, bytecode.instructions);
        try testConstants(tt.expectedConstants, bytecode.constants);
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
    std.log.warn("input: {any}", .{input});
    const l = lexer.Lexer.init(test_allocator, input);
    // defer l.deinit();
    var p = parser.Parser.init(l, test_allocator);
    // defer p.deinit();
    const prog = p.parseProgram();

    std.log.warn("prog: {any}", .{prog});
    const node = .{ .program = prog };

    return .{
        .lexer = l,
        .parser = p,
        .node = node,
    };
}

fn testInstructions(allocator: std.mem.Allocator, expected: []const code.Instructions, actual: code.Instructions) !void {
    // const concatted = try concatInstructions(allocator, expected);
    const concatted = try utils.flatten(allocator, expected);
    defer allocator.free(concatted);
    std.log.warn("concatted: {any}", .{concatted});
    std.log.warn("actual: {any}", .{actual});
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

// fn concatInstructions(allocator: std.mem.Allocator, slices: []const code.Instructions) !code.Instructions {
//     var totalLength: usize = 0;
//     for (slices) |arr| {
//         totalLength += arr.len;
//     }

//     // Allocate the flattened array
//     var flattened = try allocator.alloc(u8, totalLength);
//     defer allocator.free(flattened);

//     var offset: usize = 0;
//     for (slices) |arr| {
//         for (arr, 0..) |byte, j| {
//             flattened[offset + j] = byte;
//         }
//         offset += arr.len;
//     }

//     return flattened;
// }
