const std = @import("std");
const code = @import("code.zig");
const object = @import("object.zig");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const utils = @import("utils.zig");
const sym = @import("symbol_table.zig");
const builtins = @import("builtins.zig");

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

    // arena: std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,
    instructions: std.ArrayList(u8),
    lastInstruction: EmittedInstruction,
    previousInstruction: EmittedInstruction,

    pub fn init(allocator: std.mem.Allocator) CompilationScope {
        // var arena = std.heap.ArenaAllocator.init(allocator);
        return CompilationScope{
            // .arena = arena,
            .allocator = allocator,
            .instructions = std.ArrayList(u8).init(allocator),
            .lastInstruction = EmittedInstruction.init(@intFromEnum(code.Constants.OpConstant), 0),
            .previousInstruction = EmittedInstruction.init(@intFromEnum(code.Constants.OpConstant), 0),
        };
    }

    pub fn deinit(self: *Self) void {
        // self.arena.deinit();
        self.instructions.deinit();
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
        const arena = std.heap.ArenaAllocator.init(allocator);
        const constants = allocator.create(std.ArrayList(object.Object)) catch unreachable;
        constants.* = std.ArrayList(object.Object).init(allocator);
        const mainScope = CompilationScope.init(allocator);
        // var scopes = std.ArrayList(CompilationScope).init(allocator);
        var scopes = std.ArrayList(CompilationScope).init(allocator);
        scopes.append(mainScope) catch unreachable;

        const symbolTable = sym.SymbolTable.init(allocator);

        for (builtins.Builtins, 0..) |builtin, i| {
            _ = symbolTable.defineBuiltin(i, builtin.name) catch unreachable;
        }

        return Compiler{
            .arena = arena,
            .allocator = allocator,
            // .instructions = std.ArrayList(u8).init(allocator),
            .constants = constants,
            .definitions = definitions,
            // .lastInstruction = EmittedInstruction.init(@intFromEnum(code.Constants.OpConstant), 0),
            // .previousInstruction = EmittedInstruction.init(@intFromEnum(code.Constants.OpConstant), 0),
            .symbolTable = symbolTable,
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
        const arena = std.heap.ArenaAllocator.init(allocator);
        const mainScope = CompilationScope.init(allocator);
        // var scopes = std.ArrayList(CompilationScope).init(allocator);
        var scopes = std.ArrayList(CompilationScope).init(allocator);
        scopes.append(mainScope) catch unreachable;

        return Compiler{
            .arena = arena,
            .allocator = allocator,
            // .instructions = std.ArrayList(u8).init(arena.allocator()),
            .constants = constants,
            .definitions = definitions,
            // .lastInstruction = EmittedInstruction.init(@intFromEnum(code.Constants.OpConstant), 0),
            // .previousInstruction = EmittedInstruction.init(@intFromEnum(code.Constants.OpConstant), 0),
            .symbolTable = symbolTable,
            .scopes = scopes,
            .scopeIndex = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        // _ = self;
        // self.instructions.deinit();
        self.constants.deinit();
        self.symbolTable.deinit();
        self.allocator.destroy(self.constants);
        self.allocator.destroy(self.symbolTable);
        for (self.scopes.items) |scope| {
            var s = scope;
            s.deinit();
        }
        // self.scopes.deinit();
        // for (0..self.scopes.items.len) |i| {
        //     var scope = self.scopes.items[i];
        //     scope.deinit();
        // }
        self.scopes.deinit();
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

                        if (self.lastInstructionIs(@intFromEnum(code.Constants.OpPop))) {
                            self.removeLastPop();
                        }

                        const jumpPos = try self.emit(@intFromEnum(code.Constants.OpJump), &[_]usize{9999});

                        const afterConsequencePos = self.currentInstructions().items.len;
                        _ = try self.changeOperand(jumpNotTruthyPos, afterConsequencePos);

                        if (ie.alternative) |alternative| {
                            try self.compile(.{ .blockStatement = alternative });

                            if (self.lastInstructionIs(@intFromEnum(code.Constants.OpPop))) {
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
                            try self.loadSymbols(s);
                        } else {
                            std.debug.print("undefined variable: {s}\n", .{i.identifier});
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
                    .functionLiteral => |fl| {
                        try self.enterScope();

                        if (fl.name.len > 0) {
                            _ = try self.symbolTable.defineFunctionName(fl.name);
                        }

                        for (fl.parameters.items) |param| {
                            _ = try self.symbolTable.define(param.identifier);
                        }

                        try self.compile(.{ .blockStatement = fl.body });

                        if (self.lastInstructionIs(@intFromEnum(code.Constants.OpPop))) {
                            try self.replaceLastPopWithReturn();
                        }

                        if (!self.lastInstructionIs(@intFromEnum(code.Constants.OpReturnValue))) {
                            _ = try self.emit(@intFromEnum(code.Constants.OpReturn), &[_]usize{});
                        }

                        const freeSymbols = try self.symbolTable.freeSymbols.clone();
                        const numLocals = self.symbolTable.num_definitions;
                        var scope = try self.leaveScope();
                        const instructions = try scope.instructions.toOwnedSlice();

                        for (freeSymbols.items) |symbol| {
                            try self.loadSymbols(symbol);
                        }

                        // std.log.warn("instructions (functionLiteral): {any}", .{instructions.items});

                        const compiledFn: object.Object = .{
                            .compiledFunction = .{
                                .instructions = instructions,
                                .numLocals = numLocals,
                                .numParameters = @intCast(fl.parameters.items.len),
                            },
                        };

                        const fnIndex = try self.addConstant(compiledFn);
                        _ = try self.emit(@intFromEnum(code.Constants.OpClosure), &[_]usize{ fnIndex, freeSymbols.items.len });
                    },
                    .callExpression => |ce| {
                        try self.compile(.{ .expression = ce.function });

                        for (ce.arguments.items) |arg| {
                            try self.compile(.{ .expression = arg });
                        }

                        _ = try self.emit(@intFromEnum(code.Constants.OpCall), &[_]usize{ce.arguments.items.len});
                    },
                    else => {},
                }
            },
            .statement => |s| {
                // std.log.warn("{any}", .{node});
                switch (s.*) {
                    .letStatement => |ls| {
                        const symbol = try self.symbolTable.define(ls.identifier.identifier);
                        try self.compile(.{ .expression = ls.expression });
                        // std.log.warn("let statement: {any}", .{ls});
                        // std.log.warn("let statement after: {any}", .{symbol});
                        switch (symbol.scope) {
                            .global => {
                                _ = try self.emit(@intFromEnum(code.Constants.OpSetGlobal), &[_]usize{symbol.index});
                            },
                            else => {
                                // std.log.warn("set local: {any}", .{symbol.index});
                                _ = try self.emit(@intFromEnum(code.Constants.OpSetLocal), &[_]usize{symbol.index});
                            },
                        }
                    },
                    .returnStatement => |rs| {
                        // std.log.warn("return statement: {any}", .{rs});
                        try self.compile(.{ .expression = rs.expression });

                        _ = try self.emit(@intFromEnum(code.Constants.OpReturnValue), &[_]usize{});
                    },
                    .expressionStatement => |es| {
                        try self.compile(.{ .expression = es.expression });
                        _ = try self.emit(@intFromEnum(code.Constants.OpPop), &[_]usize{});
                    },

                    else => {
                        std.log.warn("unknown statement: {any}", .{s});
                    },
                }
            },
            .blockStatement => |bs| {
                for (bs.statements.items) |stmt| {
                    try self.compile(.{ .statement = stmt });
                }
            },
        }
    }

    fn loadSymbols(self: *Self, symbol: sym.Symbol) !void {
        switch (symbol.scope) {
            .global => {
                _ = try self.emit(@intFromEnum(code.Constants.OpGetGlobal), &[_]usize{symbol.index});
            },
            .local => {
                _ = try self.emit(@intFromEnum(code.Constants.OpGetLocal), &[_]usize{symbol.index});
            },
            .builtin => {
                _ = try self.emit(@intFromEnum(code.Constants.OpGetBuiltin), &[_]usize{symbol.index});
            },
            .free => {
                _ = try self.emit(@intFromEnum(code.Constants.OpGetFree), &[_]usize{symbol.index});
            },
            .function => {
                _ = try self.emit(@intFromEnum(code.Constants.OpCurrentClosure), &[_]usize{});
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

    fn replaceLastPopWithReturn(self: *Self) !void {
        const lastPos = self.scopes.items[self.scopeIndex].lastInstruction.position;
        _ = try self.replaceInstruction(lastPos, code.make(self.arena.allocator(), self.definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}));
        self.scopes.items[self.scopeIndex].lastInstruction.opcode = @intFromEnum(code.Constants.OpReturnValue);
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

    fn lastInstructionIs(self: *Self, op: code.Opcode) bool {
        if (self.currentInstructions().items.len == 0) {
            return false;
        }

        return self.scopes.items[self.scopeIndex].lastInstruction.opcode == op;
    }

    fn removeLastPop(self: *Self) void {
        const previous = self.scopes.items[self.scopeIndex].previousInstruction;
        _ = self.scopes.items[self.scopeIndex].instructions.pop();
        self.scopes.items[self.scopeIndex].lastInstruction = previous;
    }

    pub fn bytecode(self: *Self) Bytecode {
        // std.log.warn("instructions (bytecode): {any}", .{self.currentInstructions().items});
        // std.log.warn("constants (bytecode): {any}", .{self.constants.items});
        // std.log.warn("scopeIndex: {any}", .{self.scopeIndex});
        // std.log.warn("scopes (bytecode): {any}", .{self.scopes.items[self.scopeIndex].instructions.items});
        // std.log.warn("", .{});
        // std.log.warn("instructions (bytecode): {any}", .{self.instructions.items});
        return Bytecode{
            .instructions = self.currentInstructions().items,
            .constants = self.constants.items[0..],
        };
    }

    pub fn emit(self: *Self, op: code.Opcode, operands: []const usize) !usize {
        const ins = code.make(self.arena.allocator(), self.definitions, op, operands);
        // defer self.allocator.free(ins);
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
        const scope = CompilationScope.init(self.arena.allocator());
        // defer scope.deinit();

        // std.debug.print("scopes: {any}", .{self.scopes.items.len});
        try self.scopes.append(scope);
        // std.debug.print("scopes: {any}", .{self.scopes.items.len});
        self.scopeIndex += 1;

        self.symbolTable = sym.SymbolTable.initEnclosedScope(self.arena.allocator(), self.symbolTable);
    }

    pub fn leaveScope(self: *Self) !CompilationScope {
        // const instructions = try self.currentInstructions().clone();
        const current = self.scopes.pop();
        // std.log.warn("current.instructions: {any}", .{current.instructions.items});
        // defer top.deinit();
        self.scopeIndex -= 1;

        self.symbolTable = self.symbolTable.outer.?;

        return current;
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
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 2, 0 }),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "fn() { 5 + 10 }",
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
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 2, 0 }),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "fn() { 1; 2 }",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
                .{ .int = 2 },
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 2, 0 }),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
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

    try runCompilerTests(tests, definitions);

    for (tests) |tt| {
        for (tt.expectedInstructions) |ins| {
            test_allocator.free(ins);
        }

        for (tt.expectedConstants) |c| {
            switch (c) {
                .instructions => |i| {
                    for (i) |ins| {
                        test_allocator.free(ins);
                    }
                },
                else => {},
            }
        }
    }
}

test "test functions without return value" {
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    const tests = &[_]CompilerTestCase{
        CompilerTestCase{
            .input = "fn() { }",
            .expectedConstants = &[_]val{
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturn), &[_]usize{}),
                    },
                },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 0, 0 }),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
    };

    try runCompilerTests(tests, definitions);

    for (tests) |tt| {
        for (tt.expectedInstructions) |ins| {
            test_allocator.free(ins);
        }

        for (tt.expectedConstants) |c| {
            switch (c) {
                .instructions => |i| {
                    for (i) |ins| {
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

    const globalSymbolTable = compiler.symbolTable;

    _ = try compiler.emit(@intFromEnum(code.Constants.OpMul), &[_]usize{});

    try compiler.enterScope();

    try expectEqual(compiler.scopeIndex, 1);

    _ = try compiler.emit(@intFromEnum(code.Constants.OpSub), &[_]usize{});

    try expectEqual(compiler.scopes.items[compiler.scopeIndex].instructions.items.len, 1);

    var last = compiler.scopes.items[compiler.scopeIndex].lastInstruction;
    try expectEqual(last.opcode, @intFromEnum(code.Constants.OpSub));

    try expectEqual(compiler.symbolTable.outer, globalSymbolTable);

    var ins = try compiler.leaveScope();
    defer ins.deinit();

    try expectEqual(compiler.scopeIndex, 0);

    try expectEqual(compiler.symbolTable, globalSymbolTable);

    try expectEqual(compiler.symbolTable.outer, null);

    _ = try compiler.emit(@intFromEnum(code.Constants.OpAdd), &[_]usize{});

    try expectEqual(compiler.scopes.items[compiler.scopeIndex].instructions.items.len, 2);

    last = compiler.scopes.items[compiler.scopeIndex].lastInstruction;
    try expectEqual(last.opcode, @intFromEnum(code.Constants.OpAdd));

    const previous = compiler.scopes.items[compiler.scopeIndex].previousInstruction;
    try expectEqual(previous.opcode, @intFromEnum(code.Constants.OpMul));
}

test "test function calls" {
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    const tests = &[_]CompilerTestCase{
        CompilerTestCase{
            .input = "fn() { 24 }()",
            .expectedConstants = &[_]val{
                .{ .int = 24 },
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 1, 0 }),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpCall), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "let noArg = fn() { 24 }; noArg();",
            .expectedConstants = &[_]val{
                .{ .int = 24 },
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 1, 0 }),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetGlobal), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetGlobal), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpCall), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "let oneArg = fn(a) { a }; oneArg(24);",
            .expectedConstants = &[_]val{
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
                .{ .int = 24 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 0, 0 }),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetGlobal), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetGlobal), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpCall), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "let manyArg = fn(a, b, c) { a; b; c; }; manyArg(24, 25, 26);",
            .expectedConstants = &[_]val{
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{1}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{2}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
                .{ .int = 24 },
                .{ .int = 25 },
                .{ .int = 26 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 0, 0 }),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetGlobal), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetGlobal), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{2}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{3}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpCall), &[_]usize{3}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
    };

    try runCompilerTests(tests, definitions);

    for (tests) |tt| {
        for (tt.expectedInstructions) |ins| {
            test_allocator.free(ins);
        }

        for (tt.expectedConstants) |c| {
            switch (c) {
                .instructions => |i| {
                    for (i) |ins| {
                        test_allocator.free(ins);
                    }
                },
                else => {},
            }
        }
    }
}

test "test let statement scopes" {
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    const tests = &[_]CompilerTestCase{
        CompilerTestCase{
            .input = "let num = 55; fn() { num }",
            .expectedConstants = &[_]val{
                .{ .int = 55 },
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetGlobal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetGlobal), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 1, 0 }),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "fn() {let num = 55; num}",
            .expectedConstants = &[_]val{
                .{ .int = 55 },
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 1, 0 }),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "fn() {let a = 55; let b = 77; a + b}",
            .expectedConstants = &[_]val{
                .{ .int = 55 },
                .{ .int = 77 },
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetLocal), &[_]usize{1}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{1}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpAdd), &[_]usize{}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 2, 0 }),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
    };

    try runCompilerTests(tests, definitions);

    for (tests) |tt| {
        for (tt.expectedInstructions) |ins| {
            test_allocator.free(ins);
        }

        for (tt.expectedConstants) |c| {
            switch (c) {
                .instructions => |i| {
                    for (i) |ins| {
                        test_allocator.free(ins);
                    }
                },
                else => {},
            }
        }
    }
}

test "test built-in functions" {
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    const tests = &[_]CompilerTestCase{
        CompilerTestCase{
            .input = "len([]); push([], 1);",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetBuiltin), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpArray), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpCall), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetBuiltin), &[_]usize{5}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpArray), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpCall), &[_]usize{2}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "fn() { len([]) }",
            .expectedConstants = &[_]val{
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetBuiltin), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpArray), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpCall), &[_]usize{1}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 0, 0 }),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        // CompilerTestCase{
        //     .input = "fn() { len([1, 2, 3]) }",
        //     .expectedConstants = &[_]val{
        //         .{ .int = 1 },
        //         .{ .int = 2 },
        //         .{ .int = 3 },
        //     },
        //     .expectedInstructions = &[_]code.Instructions{
        //         code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
        //         code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
        //         code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{2}),
        //         code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpArray), &[_]usize{3}),
        //         code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpCall), &[_]usize{1}),
        //         code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
        //     },
        // },
    };

    try runCompilerTests(tests, definitions);

    for (tests) |tt| {
        for (tt.expectedInstructions) |ins| {
            test_allocator.free(ins);
        }

        for (tt.expectedConstants) |c| {
            switch (c) {
                .instructions => |i| {
                    for (i) |ins| {
                        test_allocator.free(ins);
                    }
                },
                else => {},
            }
        }
    }
}

test "test closures" {
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    const tests = &[_]CompilerTestCase{
        CompilerTestCase{
            .input = "fn(a) { fn(b) { a + b } }",
            .expectedConstants = &[_]val{
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetFree), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpAdd), &[_]usize{}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 0, 1 }),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 1, 0 }),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "fn(a) { fn(b) { fn(c) { a + b + c } } }",
            .expectedConstants = &[_]val{
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetFree), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetFree), &[_]usize{1}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpAdd), &[_]usize{}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpAdd), &[_]usize{}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetFree), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 0, 2 }),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 1, 1 }),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 2, 0 }),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "let global = 55; fn() { let a = 66; fn() { let b = 77; fn() { let c = 88; global + a + b + c } } }",
            .expectedConstants = &[_]val{
                .{ .int = 55 },
                .{ .int = 66 },
                .{ .int = 77 },
                .{ .int = 88 },
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{3}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetGlobal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetFree), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpAdd), &[_]usize{}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetFree), &[_]usize{1}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpAdd), &[_]usize{}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpAdd), &[_]usize{}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{2}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetFree), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 4, 2 }),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{1}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 5, 1 }),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetGlobal), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 6, 0 }),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
    };

    try runCompilerTests(tests, definitions);

    for (tests) |tt| {
        for (tt.expectedInstructions) |ins| {
            test_allocator.free(ins);
        }

        for (tt.expectedConstants) |c| {
            switch (c) {
                .instructions => |i| {
                    for (i) |ins| {
                        test_allocator.free(ins);
                    }
                },
                else => {},
            }
        }
    }
}

test "test recursive functions" {
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    const tests = &[_]CompilerTestCase{
        CompilerTestCase{
            .input = "let countDown = fn(x) { countDown(x - 1); }; countDown(1);",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpCurrentClosure), &[_]usize{}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSub), &[_]usize{}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpCall), &[_]usize{1}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
                .{ .int = 1 },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 1, 0 }),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetGlobal), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetGlobal), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{2}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpCall), &[_]usize{1}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
        CompilerTestCase{
            .input = "let wrapper = fn() { let countDown = fn(x) { countDown(x - 1); }; countDown(1); }; wrapper();",
            .expectedConstants = &[_]val{
                .{ .int = 1 },
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpCurrentClosure), &[_]usize{}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSub), &[_]usize{}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpCall), &[_]usize{1}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
                .{ .int = 1 },
                .{
                    .instructions = &[_]code.Instructions{
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 1, 0 }),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetLocal), &[_]usize{0}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpConstant), &[_]usize{2}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpCall), &[_]usize{1}),
                        code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpReturnValue), &[_]usize{}),
                    },
                },
            },
            .expectedInstructions = &[_]code.Instructions{
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpClosure), &[_]usize{ 3, 0 }),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpSetGlobal), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpGetGlobal), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpCall), &[_]usize{0}),
                code.make(test_allocator, definitions, @intFromEnum(code.Constants.OpPop), &[_]usize{}),
            },
        },
    };

    try runCompilerTests(tests, definitions);

    for (tests) |tt| {
        for (tt.expectedInstructions) |ins| {
            test_allocator.free(ins);
        }

        for (tt.expectedConstants) |c| {
            switch (c) {
                .instructions => |i| {
                    for (i) |ins| {
                        test_allocator.free(ins);
                    }
                },
                else => {},
            }
        }
    }
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
    // std.log.warn("input: {s}", .{input});
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
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);
    const concatted = try utils.flatten(allocator, expected);
    defer allocator.free(concatted);
    // const formattedConcatted = try code.formatInstructions(allocator, definitions, concatted);
    // defer allocator.free(formattedConcatted);
    // std.log.warn("concatted: {s}", .{formattedConcatted});

    // const formattedActual = try code.formatInstructions(allocator, definitions, actual);
    // defer allocator.free(formattedActual);
    // std.log.warn("actual: {s}", .{formattedActual});
    try expectEqual(concatted.len, actual.len);

    for (concatted, 0..) |ins, i| {
        const act = actual[i];
        try expectEqual(ins, act);
    }
}

fn testConstants(expected: []const val, actual: []const object.Object) !void {
    try std.testing.expectEqual(expected.len, actual.len);

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
    try expectEqual(i, @as(i64, @intCast(expected)));
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
