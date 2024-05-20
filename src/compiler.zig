const std = @import("std");
const code = @import("code.zig");
const object = @import("object.zig");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const utils = @import("utils.zig");
const sym = @import("symbol_table.zig");

const EmittedInstruction = struct {
    opcode: code.Opcode,
    position: usize,

    pub fn init(opcode: code.Opcode, position: usize) EmittedInstruction {
        return EmittedInstruction{
            .opcode = opcode,
            .position = position,
        };
    }
};

const CompilationScope = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,
    instructions: std.ArrayList(u8),
    lastInstruction: EmittedInstruction,
    previousInstruction: EmittedInstruction,

    pub fn init(allocator: std.mem.Allocator) CompilationScope {
        var arena = std.heap.ArenaAllocator.init(allocator);
        return CompilationScope{
            .arena = arena,
            .allocator = allocator,
            .instructions = std.ArrayList(u8).init(arena.allocator()),
            .lastInstruction = EmittedInstruction.init(@intFromEnum(code.Constants.OpConstant), 0),
            .previousInstruction = EmittedInstruction.init(@intFromEnum(code.Constants.OpConstant), 0),
        };
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }
};

pub const Compiler = struct {
    const Self = @This();
    arena: std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,
    // instructions: std.ArrayList(u8),
    constants: *std.ArrayList(object.Object),
    definitions: code.Definitions,
    // lastInstruction: EmittedInstruction,
    // previousInstruction: EmittedInstruction,
    symbolTable: *sym.SymbolTable,
    scopes: std.ArrayList(CompilationScope),
    scopeIndex: usize,

    pub fn init(allocator: std.mem.Allocator, definitions: code.Definitions) Compiler {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const constants = allocator.create(std.ArrayList(object.Object)) catch unreachable;
        constants.* = std.ArrayList(object.Object).init(allocator);
        const mainScope = CompilationScope.init(allocator);
        // var scopes = std.ArrayList(CompilationScope).init(allocator);
        var scopes = std.ArrayList(CompilationScope).init(arena.allocator());
        scopes.append(mainScope) catch unreachable;
        return Compiler{
            .arena = arena,
            .allocator = allocator,
            // .instructions = std.ArrayList(u8).init(allocator),
            .constants = constants,
            .definitions = definitions,
            // .lastInstruction = EmittedInstruction.init(@intFromEnum(code.Constants.OpConstant), 0),
            // .previousInstruction = EmittedInstruction.init(@intFromEnum(code.Constants.OpConstant), 0),
            .symbolTable = sym.SymbolTable.init(allocator),
            .scopes = scopes,
            .scopeIndex = 0,
        };
    }

    pub fn initWithState(
        allocator: std.mem.Allocator,
        definitions: code.Definitions,
        symbolTable: *sym.SymbolTable,
        constants: *std.ArrayList(object.Object),
    ) Compiler {
        var arena = std.heap.ArenaAllocator.init(allocator);
        return Compiler{
            .arena = arena,
            .allocator = allocator,
            .instructions = std.ArrayList(u8).init(arena.allocator()),
            .constants = constants,
            .definitions = definitions,
            .lastInstruction = EmittedInstruction.init(@intFromEnum(code.Constants.OpConstant), 0),
            .previousInstruction = EmittedInstruction.init(@intFromEnum(code.Constants.OpConstant), 0),
            .symbolTable = symbolTable,
        };
    }

    pub fn deinit(self: *Self) void {
        // _ = self;
        // self.instructions.deinit();
        self.constants.deinit();
        self.symbolTable.deinit();
        self.allocator.destroy(self.constants);
        self.allocator.destroy(self.symbolTable);
        // for (self.scopes.items) |scope| {
        //     scope.instructions.deinit();
        // }
        // self.scopes.deinit();
        for (0..self.scopes.items.len) |i| {
            var scope = self.scopes.items[i];
            scope.deinit();
        }
        self.arena.deinit();
    }

    pub fn compile(self: *Self, node: ast.Node) !void {
        // std.log.warn("node (compile): {any}", .{node});
        switch (node) {
            .program => |p| {
                for (p.statements.items) |stmt| {
                    try self.compile(.{ .statement = stmt });
                }
            },
            .expression => |e| {
                switch (e.*) {
                    .infix => |ie| {
                        if (std.mem.eql(u8, ie.operator, "<")) {
                            try self.compile(.{ .expression = ie.right });
                            try self.compile(.{ .expression = ie.left });
                            _ = try self.emit(@intFromEnum(code.Constants.OpGreaterThan), &[_]usize{});
                            return;
                        }

                        try self.compile(.{ .expression = ie.left });
                        try self.compile(.{ .expression = ie.right });

                        if (std.mem.eql(u8, ie.operator, "+")) {
                            _ = try self.emit(@intFromEnum(code.Constants.OpAdd), &[_]usize{});
                        } else if (std.mem.eql(u8, ie.operator, "-")) {
                            _ = try self.emit(@intFromEnum(code.Constants.OpSub), &[_]usize{});
                        } else if (std.mem.eql(u8, ie.operator, "*")) {
                            _ = try self.emit(@intFromEnum(code.Constants.OpMul), &[_]usize{});
                        } else if (std.mem.eql(u8, ie.operator, "/")) {
                            _ = try self.emit(@intFromEnum(code.Constants.OpDiv), &[_]usize{});
                        } else if (std.mem.eql(u8, ie.operator, ">")) {
                            _ = try self.emit(@intFromEnum(code.Constants.OpGreaterThan), &[_]usize{});
                        } else if (std.mem.eql(u8, ie.operator, "==")) {
                            _ = try self.emit(@intFromEnum(code.Constants.OpEqual), &[_]usize{});
                        } else if (std.mem.eql(u8, ie.operator, "!=")) {
                            _ = try self.emit(@intFromEnum(code.Constants.OpNotEqual), &[_]usize{});
                        } else {
                            std.debug.print("unknown operator: {any}", .{ie.operator});
                        }
                    },
                    .prefix => |pe| {
                        if (std.mem.eql(u8, pe.operator, "-")) {
                            try self.compile(.{ .expression = pe.right });
                            _ = try self.emit(@intFromEnum(code.Constants.OpMinus), &[_]usize{});
                        } else if (std.mem.eql(u8, pe.operator, "!")) {
                            try self.compile(.{ .expression = pe.right });
                            _ = try self.emit(@intFromEnum(code.Constants.OpBang), &[_]usize{});
                        }
                    },
                    .integer => |i| {
                        const integer = object.Object{ .integer = i };
                        const c = try self.addConstant(integer);
                        _ = try self.emit(@intFromEnum(code.Constants.OpConstant), &[_]usize{c});
                    },
                    .boolean => |b| {
                        if (b) {
                            _ = try self.emit(@intFromEnum(code.Constants.OpTrue), &[_]usize{});
                        } else {
                            _ = try self.emit(@intFromEnum(code.Constants.OpFalse), &[_]usize{});
                        }
                    },
                    .ifExpression => |ie| {
                        try self.compile(.{ .expression = ie.condition });

                        const jumpNotTruthyPos = try self.emit(@intFromEnum(code.Constants.OpJumpNotTruthy), &[_]usize{9999});

                        try self.compile(.{ .blockStatement = ie.consequence });

                        if (self.lastInstructionIsPop()) {
                            self.removeLastPop();
                        }

                        const jumpPos = try self.emit(@intFromEnum(code.Constants.OpJump), &[_]usize{9999});

                        const afterConsequencePos = self.currentInstructions().items.len;
                        _ = try self.changeOperand(jumpNotTruthyPos, afterConsequencePos);

                        if (ie.alternative) |alternative| {
                            try self.compile(.{ .blockStatement = alternative });

                            if (self.lastInstructionIsPop()) {
                                self.removeLastPop();
                            }
                        } else {
                            _ = try self.emit(@intFromEnum(code.Constants.OpNull), &[_]usize{});
                        }

                        const afterAlternativePos = self.currentInstructions().items.len;
                        _ = try self.changeOperand(jumpPos, afterAlternativePos);
                    },
                    .identifier => |i| {
                        const symbol = self.symbolTable.resolve(i.identifier);
                        if (symbol) |s| {
                            _ = try self.emit(@intFromEnum(code.Constants.OpGetGlobal), &[_]usize{s.index});
                        } else {
                            std.debug.print("undefined variable: {any}", .{i.identifier});
                            return;
                        }
                    },
                    .stringLiteral => |s| {
                        const str = object.Object{ .string = s };
                        const c = try self.addConstant(str);
                        _ = try self.emit(@intFromEnum(code.Constants.OpConstant), &[_]usize{c});
                    },
                    .arrayLiteral => |al| {
                        for (al.elements.items) |elem| {
                            try self.compile(.{ .expression = elem });
                        }

                        _ = try self.emit(@intFromEnum(code.Constants.OpArray), &[_]usize{al.elements.items.len});
                    },
                    .hashLiteral => |hl| {
                        var keys = std.ArrayList(*ast.Expression).init(self.arena.allocator());
                        var it = hl.pairs.iterator();
                        while (it.next()) |pair| {
                            try keys.append(pair.key_ptr.*);
                        }

                        std.mem.sort(*ast.Expression, keys.items, {}, cmpExpression);

                        for (keys.items) |key| {
                            try self.compile(.{ .expression = key });
                            try self.compile(.{ .expression = hl.pairs.get(key).? });
                        }

                        _ = try self.emit(@intFromEnum(code.Constants.OpHash), &[_]usize{keys.items.len * 2});
                    },
                    .indexExpression => |ie| {
                        try self.compile(.{ .expression = ie.left });
                        try self.compile(.{ .expression = ie.index });
                        _ = try self.emit(@intFromEnum(code.Constants.OpIndex), &[_]usize{});
                    },
                    else => {},
                }
            },
            .statement => |s| {
                // std.log.warn("{any}", .{node});
                switch (s.*) {
                    .letStatement => |ls| {
                        try self.compile(.{ .expression = ls.expression });
                        const symbol = try self.symbolTable.define(ls.identifier.identifier);
                        _ = try self.emit(@intFromEnum(code.Constants.OpSetGlobal), &[_]usize{symbol.index});
                    },
                    .expressionStatement => |es| {
                        try self.compile(.{ .expression = es.expression });
                        _ = try self.emit(@intFromEnum(code.Constants.OpPop), &[_]usize{});
                    },

                    else => {},
                }
            },
            .blockStatement => |bs| {
                for (bs.statements.items) |stmt| {
                    try self.compile(.{ .statement = stmt });
                }
            },
        }
    }

    fn currentInstructions(self: *Self) *std.ArrayList(u8) {
        return &self.scopes.items[self.scopeIndex].instructions;
    }

    fn cmpExpression(context: void, a: *ast.Expression, b: *ast.Expression) bool {
        _ = context;
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const aStr = std.fmt.allocPrint(gpa.allocator(), "{s}", .{a}) catch return false;
        const bStr = std.fmt.allocPrint(gpa.allocator(), "{s}", .{b}) catch return false;
        defer gpa.allocator().free(aStr);
        defer gpa.allocator().free(bStr);
        return std.mem.lessThan(u8, aStr, bStr);
    }

    fn replaceInstruction(self: *Self, pos: usize, newInstruction: []const u8) !void {
        const ins = self.currentInstructions();
        var i: usize = 0;
        while (i < newInstruction.len) : (i += 1) {
            ins.items[pos + i] = newInstruction[i];
        }
    }

    fn changeOperand(self: *Self, pos: usize, operand: usize) !void {
        const op = self.currentInstructions().items[pos];
        const newInstruction = code.make(self.arena.allocator(), self.definitions, op, &[_]usize{operand});

        return self.replaceInstruction(pos, newInstruction);
    }

    fn lastInstructionIsPop(self: Self) bool {
        return self.scopes.items[self.scopeIndex].lastInstruction.opcode == @intFromEnum(code.Constants.OpPop);
    }

    fn removeLastPop(self: *Self) void {
        const previous = self.scopes.items[self.scopeIndex].previousInstruction;
        _ = self.scopes.items[self.scopeIndex].instructions.pop();
        self.scopes.items[self.scopeIndex].lastInstruction = previous;
    }

    pub fn bytecode(self: *Self) Bytecode {
        // std.log.warn("instructions (bytecode): {any}", .{self.instructions.items});
        return Bytecode{
            .instructions = self.currentInstructions().items[0..],
            .constants = self.constants.items[0..],
        };
    }

    pub fn emit(self: *Self, op: code.Opcode, operands: []const usize) !usize {
        const ins = code.make(self.arena.allocator(), self.definitions, op, operands);
        const pos = try self.addInstruction(ins);
        self.setLastInstruction(op, pos);

        return pos;
    }

    fn setLastInstruction(self: *Self, op: code.Opcode, pos: usize) void {
        const previous = self.scopes.items[self.scopeIndex].lastInstruction;
        const last = EmittedInstruction.init(op, pos);

        self.scopes.items[self.scopeIndex].previousInstruction = previous;
        self.scopes.items[self.scopeIndex].lastInstruction = last;
    }

    pub fn addInstruction(self: *Self, ins: []const u8) !usize {
        // std.log.warn("ins (addIns): {any}", .{ins});
        const posNewInstruction = self.currentInstructions().items.len;
        // var updatedInstructions = try self.currentInstructions().clone();
        // try updatedInstructions.appendSlice(ins);
        // std.debug.print("ins: {any}", .{ins});
        // std.debug.print("instructions: {any}", .{self.scopes.items[self.scopeIndex].instructions.items});
        // for (ins) |i| {
        //     try self.scopes.items[self.scopeIndex].instructions.append(i);
        // }
        try self.scopes.items[self.scopeIndex].instructions.appendSlice(ins);
        // self.scopes.items[self.scopeIndex].instructions = updatedInstructions;
        return posNewInstruction;
    }

    pub fn addConstant(self: *Self, obj: object.Object) !usize {
        try self.constants.append(obj);
        return self.constants.items.len - 1;
    }

    pub fn enterScope(self: *Self) !void {
        // const instructions = std.ArrayList(u8).init(self.arena.allocator());
        const scope = CompilationScope.init(self.allocator);

        std.debug.print("scopes: {any}", .{self.scopes.items.len});
        try self.scopes.append(scope);
        std.debug.print("scopes: {any}", .{self.scopes.items.len});
        self.scopeIndex += 1;
    }

    pub fn leaveScope(self: *Self) *std.ArrayList(u8) {
        _ = self.scopes.pop();
        self.scopeIndex -= 1;

        return self.currentInstructions();
    }

    pub const Bytecode = struct {
        instructions: code.Instructions,
        constants: []const object.Object,
    };
};

const val = union(enum) {
    int: usize,
    str: []const u8,
    instructions: []const code.Instructions,
};

const CompilerTestCase = struct {
    input: []const u8,
    expectedConstants: []const val,
    expectedInstructions: []const code.Instructions,
};

const test_allocator = std.testing.allocator;
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;

test "test integer arithmetic" {
    // std.log.warn("op: {any}", .{@intFromEnum(code.Constants.OpConstant)});

    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);
    const tests = &[_]CompilerTestCase{
        CompilerTestCase{
            .input = "1 + 2",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
                .{ .int = 2 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpAdd), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "1; 2",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
                .{ .int = 2 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "1 - 2",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
                .{ .int = 2 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSub), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "1 * 2",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
                .{ .int = 2 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpMul), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "2 / 1",
            .expectedConstants = &[_]val{
                .{ .int = 2 },
                .{ .int = 1 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpDiv), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "-1",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpMinus), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
    };

    try runCompilerTests(tests, definitions);

    for (tests) |tt| {
        for (tt.expectedInstructions) |ins| {
            test_allocator.free(ins);
        }
    }
}

test "test boolean expressions" {
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);
    const tests = &[_]CompilerTestCase{
        CompilerTestCase{
            .input = "true",
            .expectedConstants = &[_]val{},
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpTrue), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "false",
            .expectedConstants = &[_]val{},
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpFalse), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "1 > 2",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
                .{ .int = 2 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGreaterThan), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "1 < 2",
            .expectedConstants = &[_]val{
                .{ .int = 2 },
                .{ .int = 1 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGreaterThan), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "1 == 2",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
                .{ .int = 2 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpEqual), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "1 != 2",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
                .{ .int = 2 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpNotEqual), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "true == false",
            .expectedConstants = &[_]val{},
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpTrue), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpFalse), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpEqual), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "true != false",
            .expectedConstants = &[_]val{},
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpTrue), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpFalse), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpNotEqual), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "!true",
            .expectedConstants = &[_]val{},
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpTrue), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpBang), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
    };

    try runCompilerTests(tests, definitions);

    for (tests) |tt| {
        for (tt.expectedInstructions) |ins| {
            test_allocator.free(ins);
        }
    }
}

test "test conditionals" {
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);
    const tests = &[_]CompilerTestCase{
        CompilerTestCase{
            .input = "if (true) { 10 }; 3333;",
            .expectedConstants = &[_]val{
                .{ .int = 10 },
                .{ .int = 3333 },
            },
            .expectedInstructions = &[_]code.Instructions{
                // 0000
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpTrue), &[_]usize{}),
                // 0001
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpJumpNotTruthy), &[_]usize{10}),
                // 0004
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                // 0007
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpJump), &[_]usize{11}),
                // 0010
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpNull), &[_]usize{}),
                // 0011
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
                // 0012
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                // 0015
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "if (true) { 10 } else { 20 }; 3333;",
            .expectedConstants = &[_]val{
                .{ .int = 10 },
                .{ .int = 20 },
                .{ .int = 3333 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpTrue), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpJumpNotTruthy), &[_]usize{10}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpJump), &[_]usize{13}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{2}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
    };

    try runCompilerTests(tests, definitions);

    for (tests) |tt| {
        for (tt.expectedInstructions) |ins| {
            test_allocator.free(ins);
        }
    }
}

test "test global let statement" {
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);
    const tests = &[_]CompilerTestCase{
        CompilerTestCase{
            .input = "let one = 1; let two = 2;",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
                .{ .int = 2 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetGlobal), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetGlobal), &[_]usize{1}),
            },
        },
        CompilerTestCase{
            .input = "let one = 1; one;",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetGlobal), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetGlobal), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "let one = 1; let two = one; two;",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetGlobal), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetGlobal), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetGlobal), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetGlobal), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
    };

    try runCompilerTests(tests, definitions);

    for (tests) |tt| {
        for (tt.expectedInstructions) |ins| {
            test_allocator.free(ins);
        }
    }
}

test "test string expressions" {
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    const tests = &[_]CompilerTestCase{
        CompilerTestCase{
            .input = "\"monkey\"",
            .expectedConstants = &[_]val{
                .{ .str = "monkey" },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "\"mon\" + \"key\"",
            .expectedConstants = &[_]val{
                .{ .str = "mon" },
                .{ .str = "key" },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpAdd), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
    };

    try runCompilerTests(tests, definitions);

    for (tests) |tt| {
        for (tt.expectedInstructions) |ins| {
            test_allocator.free(ins);
        }
    }
}

test "test array literals" {
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    const tests = &[_]CompilerTestCase{
        CompilerTestCase{
            .input = "[]",
            .expectedConstants = &[_]val{},
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpArray), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "[1, 2, 3]",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
                .{ .int = 2 },
                .{ .int = 3 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{2}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpArray), &[_]usize{3}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "[1 + 2, 3 - 4, 5 * 6]",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
                .{ .int = 2 },
                .{ .int = 3 },
                .{ .int = 4 },
                .{ .int = 5 },
                .{ .int = 6 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpAdd), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{2}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{3}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSub), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{4}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{5}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpMul), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpArray), &[_]usize{3}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
    };

    try runCompilerTests(tests, definitions);

    for (tests) |tt| {
        for (tt.expectedInstructions) |ins| {
            test_allocator.free(ins);
        }
    }
}

test "test hash literals" {
    // std.log.warn("test hash literals", .{});
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    const tests = &[_]CompilerTestCase{
        CompilerTestCase{
            .input = "{}",
            .expectedConstants = &[_]val{},
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpHash), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "{1: 2, 3: 4, 5: 6}",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
                .{ .int = 2 },
                .{ .int = 3 },
                .{ .int = 4 },
                .{ .int = 5 },
                .{ .int = 6 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{2}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{3}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{4}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{5}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpHash), &[_]usize{6}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "{1: 2 + 3, 4: 5 * 6}",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
                .{ .int = 2 },
                .{ .int = 3 },
                .{ .int = 4 },
                .{ .int = 5 },
                .{ .int = 6 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{2}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpAdd), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{3}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{4}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{5}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpMul), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpHash), &[_]usize{4}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
    };

    try runCompilerTests(tests, definitions);

    for (tests) |tt| {
        for (tt.expectedInstructions) |ins| {
            test_allocator.free(ins);
        }
    }
}

test "test index expressions" {
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    const tests = &[_]CompilerTestCase{
        CompilerTestCase{
            .input = "[1, 2, 3][1 + 1]",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
                .{ .int = 2 },
                .{ .int = 3 },
                .{ .int = 1 },
                .{ .int = 1 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{2}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpArray), &[_]usize{3}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{3}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{4}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpAdd), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpIndex), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "{1: 2}[2 - 1]",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
                .{ .int = 2 },
                .{ .int = 2 },
                .{ .int = 1 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpHash), &[_]usize{2}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{2}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{3}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSub), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpIndex), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
    };

    try runCompilerTests(tests, definitions);

    for (tests) |tt| {
        for (tt.expectedInstructions) |ins| {
            test_allocator.free(ins);
        }
    }
}

test "test functions" {
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    const tests = &[_]CompilerTestCase{
        CompilerTestCase{
            .input = "fn() { return 5 + 10 }",
            .expectedConstants = &[_]val{
                .{ .int = 5 },
                .{ .int = 10 },
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpAdd), &[_]usize{}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{2}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        // CompilerTestCase{
        //     .input = "fn() { 5 + 10 }",
        //     .expectedConstants = &[_]val{
        //         .{ .int = 5 },
        //         .{ .int = 10 },
        //     },
        //     .expectedInstructions = &[_]code.Instructions{
        //         code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
        //         code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
        //         code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpAdd), &[_]usize{}),
        //         code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
        //     },
        // },
        // CompilerTestCase{
        //     .input = "fn() { 24 }",
        //     .expectedConstants = &[_]val{
        //         .{ .int = 24 },
        //     },
        //     .expectedInstructions = &[_]code.Instructions{
        //         code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
        //         code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
        //     },
        // },
    };

    // try runCompilerTests(tests, definitions);

    for (tests) |tt| {
        for (tt.expectedInstructions) |ins| {
            test_allocator.free(ins);
        }

        for (tt.expectedConstants) |c| {
            switch (c) {
                .instructions => {
                    for (c.instructions) |ins| {
                        test_allocator.free(ins);
                    }
                },
                else => {},
            }
        }
    }
}

test "test compiler scopes" {
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    var compiler = Compiler.init(test_allocator, definitions);
    defer compiler.deinit();

    try expectEqual(compiler.scopeIndex, 0);

    _ = try compiler.emit(@intFromEnum(code.Constants.OpMul), &[_]usize{});

    try compiler.enterScope();

    try expectEqual(compiler.scopeIndex, 1);

    _ = try compiler.emit(@intFromEnum(code.Constants.OpSub), &[_]usize{});

    try expectEqual(compiler.scopes.items[compiler.scopeIndex].instructions.items.len, 1);

    var last = compiler.scopes.items[compiler.scopeIndex].lastInstruction;
    try expectEqual(last.opcode, @intFromEnum(code.Constants.OpSub));

    _ = compiler.leaveScope();
    try expectEqual(compiler.scopeIndex, 0);

    _ = try compiler.emit(@intFromEnum(code.Constants.OpAdd), &[_]usize{});

    try expectEqual(compiler.scopes.items[compiler.scopeIndex].instructions.items.len, 2);

    last = compiler.scopes.items[compiler.scopeIndex].lastInstruction;
    try expectEqual(last.opcode, @intFromEnum(code.Constants.OpAdd));

    const previous = compiler.scopes.items[compiler.scopeIndex].previousInstruction;
    try expectEqual(previous.opcode, @intFromEnum(code.Constants.OpMul));
}

fn runCompilerTests(tests: []const CompilerTestCase, definitions: code.Definitions) !void {
    // std.log.warn("runCompilerTests", .{});
    for (tests) |tt| {
        var helper = parse(tt.input);
        defer helper.deinit();

        var compiler = Compiler.init(test_allocator, definitions);
        defer compiler.deinit();
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

fn testInstructions(allocator: std.mem.Allocator, expected: []const code.Instructions, actual: code.Instructions) !void {
    // const concatted = try concatInstructions(allocator, expected);
    const concatted = try utils.flatten(allocator, expected);
    defer allocator.free(concatted);
    // std.log.warn("concatted: {any}", .{concatted});
    // std.log.warn("actual: {any}", .{actual});
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
            .str => |s| {
                const str = act.stringValue();
                assert(std.mem.eql(u8, s, str));
            },
            .instructions => |ins| {
                const compiledFn = act.compiledFunction;

                try testInstructions(test_allocator, ins, compiledFn.instructions);
                // const concatted = try utils.flatten(test_allocator, ins);
                // defer test_allocator.free(concatted);
                // std.log.warn("concatted: {any}", .{concatted});
                // std.log.warn("actual: {any}", .{act.instructions});
                // assert(concatted.len == act.instructions.len);

                // for (concatted, act.instructions) |expected, actual| {
                //     assert(expected == actual);
                // }
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
