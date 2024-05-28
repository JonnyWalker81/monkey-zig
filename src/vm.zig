const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const object = @import("object.zig");
const code = @import("code.zig");
const compiler = @import("compiler.zig");
const ArrayList = std.ArrayList;
const frame = @import("frame.zig");
const builtins = @import("builtins.zig");

pub const StackSize = 2048;
pub const GlobalSize = 65536;
pub const MaxFrames = 1024;

const True = object.Object{ .boolean = true };
const False = object.Object{ .boolean = false };
const Null: object.Object = .nil;

const VMError = error{
    WrongNumberOfArguments,
    CallingInvalidFunction,
};

pub const VM = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    constants: []const object.Object,
    globals: *[GlobalSize]object.Object,

    stack: [StackSize]object.Object,
    sp: usize,

    frames: [MaxFrames]frame.Frame,
    framesIndex: usize,

    pub fn init(allocator: std.mem.Allocator, bytecode: compiler.Compiler.Bytecode) Self {
        var arena = std.heap.ArenaAllocator.init(allocator);
        var stack: [StackSize]object.Object = undefined;
        @memset(&stack, .nil);

        var globals: [GlobalSize]object.Object = undefined;
        @memset(&globals, .nil);

        var frames: [MaxFrames]frame.Frame = undefined;
        @memset(&frames, undefined);

        const mainFn = object.Object{
            .compiledFunction = .{
                .instructions = bytecode.instructions,
                .numLocals = 0,
                .numParameters = 0,
            },
        };

        const free = ArrayList(object.Object).init(arena.allocator());
        const mainClosure = object.Object{
            .closure = .{
                .func = mainFn.compiledFn(),
                .free = free,
            },
        };

        const mainFrame = frame.Frame.init(mainClosure, 0);

        frames[0] = mainFrame;

        return .{
            .arena = arena,
            .constants = bytecode.constants,
            .globals = &globals,
            .stack = stack,
            .sp = 0,
            .frames = frames,
            .framesIndex = 1,
        };
    }

    pub fn initWithGlobalStore(allocator: std.mem.Allocator, bytecode: compiler.Compiler.Bytecode, globals: *[GlobalSize]object.Object) Self {
        const arena = std.heap.ArenaAllocator.init(allocator);
        var stack: [StackSize]object.Object = undefined;
        @memset(&stack, .nil);

        var frames: [MaxFrames]frame.Frame = undefined;
        @memset(&frames, undefined);

        const compiledFn = object.Object{ .compiledFunction = .{
            .instructions = bytecode.instructions,
            .numLocals = 0,
            .numParameters = 0,
        } };
        const mainFrame = frame.Frame.init(compiledFn, 0);

        frames[0] = mainFrame;

        return .{
            .arena = arena,
            .constants = bytecode.constants,
            // .instructions = bytecode.instructions,
            .globals = globals,
            .stack = stack,
            .sp = 0,
            .frames = frames,
            .framesIndex = 1,
        };
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
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

    pub fn currentFrame(self: *Self) *frame.Frame {
        return &self.frames[self.framesIndex - 1];
    }

    pub fn pushFrame(self: *Self, f: frame.Frame) void {
        self.frames[self.framesIndex] = f;
        self.framesIndex += 1;
    }

    pub fn popFrame(self: *Self) frame.Frame {
        self.framesIndex -= 1;
        return self.frames[self.framesIndex];
    }

    pub fn run(self: *Self) !void {
        while (self.currentFrame().ip < (self.currentFrame().instructions().len - 1)) {
            self.currentFrame().ip += 1;
            const ip: usize = @intCast(self.currentFrame().ip);
            const ins = self.currentFrame().instructions();
            const op = @as(code.Constants, @enumFromInt(ins[ip]));

            switch (op) {
                .OpConstant => {
                    // var buf: [2]u8 = undefined;
                    // const start = ip + 1;
                    // std.log.warn("start: {d}", .{start});
                    // std.log.warn("instructions: {d}", .{self.instructions});
                    // @memcpy(&buf, self.instructions[start .. start + 2]);
                    // const constIndex = std.mem.readInt(u16, &buf, .big);
                    const constIndex = readUint16(ins, ip + 1);
                    self.currentFrame().ip += 2;

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
                    const pos = readUint16(ins, ip + 1);
                    self.currentFrame().ip += 2;

                    const condition = self.pop();
                    if (!condition.boolValue()) {
                        self.currentFrame().ip = pos - 1;
                    }
                },
                .OpJump => {
                    // var buf: [2]u8 = undefined;
                    // @memcpy(&buf, self.instructions[ip + 1 .. ip + 3]);
                    // const pos = std.mem.readInt(u16, &buf, .big);
                    const pos = readUint16(ins, ip + 1);
                    self.currentFrame().ip = pos - 1;
                },
                .OpGetGlobal => {
                    const globalIndex = readUint16(ins, ip + 1);
                    self.currentFrame().ip += 2;

                    const obj = self.globals[globalIndex];
                    try self.push(obj);
                },
                .OpSetGlobal => {
                    const globalIndex = readUint16(ins, ip + 1);
                    self.currentFrame().ip += 2;

                    self.globals[globalIndex] = self.pop();
                },
                .OpArray => {
                    const numElements = readUint16(ins, ip + 1);
                    self.currentFrame().ip += 2;

                    const array = try self.buildArray(self.sp - numElements, self.sp);
                    self.sp -= numElements;

                    try self.push(array);
                },
                .OpHash => {
                    const numElements = readUint16(ins, ip + 1);
                    self.currentFrame().ip += 2;

                    const hash = try self.buildHash(self.sp - numElements, self.sp);
                    self.sp -= numElements;

                    try self.push(hash);
                },
                .OpIndex => {
                    const index = self.pop();
                    const left = self.pop();

                    try self.executeIndexExpression(left, index);
                },
                .OpCall => {
                    const numArgs = ins[ip + 1];
                    self.currentFrame().ip += 1;

                    try self.executeCall(numArgs);
                },
                .OpReturnValue => {
                    const returnValue = self.pop();
                    const f = self.popFrame();
                    self.sp = @intCast(f.basePointer - 1);

                    try self.push(returnValue);
                },
                .OpReturn => {
                    const f = self.popFrame();
                    self.sp = @intCast(f.basePointer - 1);

                    try self.push(Null);
                },
                .OpSetLocal => {
                    const localIndex = ins[ip + 1];
                    self.currentFrame().ip += 1;

                    const f = self.currentFrame();

                    self.stack[@intCast(f.basePointer + localIndex)] = self.pop();
                },
                .OpGetLocal => {
                    const localIndex = ins[ip + 1];
                    self.currentFrame().ip += 1;

                    const f = self.currentFrame();
                    try self.push(self.stack[@intCast(f.basePointer + localIndex)]);
                },
                .OpGetBuiltin => {
                    const builtinIndex = ins[ip + 1];
                    self.currentFrame().ip += 1;

                    const definition = builtins.Builtins[builtinIndex];
                    try self.push(definition.builtin);
                },
                .OpClosure => {
                    const constIndex = readUint16(ins, ip + 1);
                    const numFree = ins[ip + 3];
                    self.currentFrame().ip += 3;

                    try self.pushClosure(constIndex, @intCast(numFree));
                },
                .OpGetFree => {
                    const freeIndex = ins[ip + 1];
                    self.currentFrame().ip += 1;

                    const currentClosure = self.currentFrame().cl.closureFn();
                    try self.push(currentClosure.free.items[freeIndex]);
                },
                .OpCurrentClosure => {
                    const currentClosure = self.currentFrame().cl;
                    try self.push(currentClosure);
                },
                .OpNull => {
                    try self.push(Null);
                },
            }
        }
    }

    fn readUint16(s: []const u8, ip: usize) u16 {
        var buf: [2]u8 = undefined;
        @memcpy(&buf, s[ip .. ip + 2]);
        return std.mem.readInt(u16, &buf, .big);
    }

    fn pushClosure(self: *Self, constIndex: u16, numFree: usize) !void {
        const constant = self.constants[constIndex];
        const function = constant.compiledFn();
        var free = ArrayList(object.Object).init(self.arena.allocator());

        for (0..numFree) |i| {
            // const free = self.currentFrame().instructions()[freeIndex];
            const obj = self.stack[@intCast(self.sp - numFree + i)];
            try free.append(obj);
        }

        self.sp -= numFree;

        const closure = object.Object{ .closure = .{ .func = function, .free = free } };
        try self.push(closure);
    }

    fn callFunction(self: *Self, callee: *const object.Object, numArgs: u8) !void {
        const func = callee.compiledFn();
        if (numArgs != func.numParameters) {
            // return std.fmt.allocPrint(self.arena.allocator(), "wrong number of arguments: want={d}, got={d}", .{ f.numParameters, numArgs });
            return VMError.WrongNumberOfArguments;
        }

        const f = frame.Frame.init(callee.*, @intCast(self.sp - numArgs));
        self.pushFrame(f);
        self.sp = @as(usize, @intCast(f.basePointer + func.numLocals));
    }

    fn callBuiltin(self: *Self, builtin: *const object.Object, numArgs: u8) !void {
        const func = builtin.builtinFn();
        const args = self.stack[self.sp - numArgs .. self.sp];
        var argPtrs = std.ArrayList(*object.Object).init(self.arena.allocator());
        for (args) |arg| {
            const a = try self.arena.allocator().create(object.Object);
            a.* = arg;
            try argPtrs.append(a);
        }

        const s = try argPtrs.toOwnedSlice();
        const result = func(self.arena.allocator(), s);
        self.sp = self.sp - numArgs - 1;

        if (result) |r| {
            try self.push(r.*);
        } else {
            try self.push(.nil);
        }
    }

    fn callClosure(self: *Self, cl: *const object.Object, numArgs: u8) !void {
        const closure = cl.closureFn();
        if (numArgs != closure.func.numParameters) {
            // return std.fmt.allocPrint(self.arena.allocator(), "wrong number of arguments: want={d}, got={d}", .{ f.numParameters, numArgs });
            return VMError.WrongNumberOfArguments;
        }
        // const f = closure.closureFn();
        // const c = closure.closureFree();
        const newFrame = frame.Frame.init(cl.*, @intCast(self.sp - numArgs));
        self.pushFrame(newFrame);
        self.sp = @as(usize, @intCast(newFrame.basePointer + closure.func.numLocals));
    }

    fn executeCall(self: *Self, numArgs: u8) !void {
        const callee = self.stack[self.sp - 1 - numArgs];
        switch (callee) {
            .closure => {
                return try self.callClosure(&callee, numArgs);
            },
            .builtin => {
                // const builtin = callee.builtin;
                // const args = &self.stack[self.sp - numArgs .. self.sp];
                // const result = try builtin.fn(self.arena.allocator(), args);
                // self.sp -= numArgs + 1;
                // try self.push(result);
                return try self.callBuiltin(&callee, numArgs);
            },
            else => {
                // return std.debug.panic("calling non-function: {s}", .{callee.typeId()});
                return VMError.CallingInvalidFunction;
            },
        }
    }

    fn executeIndexExpression(self: *Self, left: object.Object, index: object.Object) !void {
        if (left == .array and index == .integer) {
            return try self.executeArrayIndex(left, index);
        }

        if (left == .hash) {
            return try self.executeHashIndex(left, index);
        }

        return std.debug.panic("index operator not supported: {s}", .{left.typeId()});
    }

    fn executeArrayIndex(self: *Self, left: object.Object, index: object.Object) !void {
        const array = left.array;
        const i = index.intValue();
        const max: i64 = @as(i64, @intCast(array.items.len)) - 1;
        if (i < 0 or i > max) {
            try self.push(Null);
            return;
        }

        try self.push(array.items[@intCast(i)].*);
    }

    fn executeHashIndex(self: *Self, left: object.Object, index: object.Object) !void {
        const hash = left.hash;
        const key = index.hashKey();
        const pair = hash.pairs.get(key);
        if (pair == null) {
            try self.push(Null);
            return;
        }

        try self.push(pair.?.value.*);
    }

    fn buildHash(self: *Self, startIndex: usize, endIndex: usize) !object.Object {
        var pairs = std.HashMap(object.HashKey, object.HashPair, object.HashKeyContext, std.hash_map.default_max_load_percentage).init(self.arena.allocator());

        var i = startIndex;
        while (i < endIndex) : (i += 2) {
            const key = try self.arena.allocator().create(object.Object);
            key.* = self.stack[i];
            const value = try self.arena.allocator().create(object.Object);
            value.* = self.stack[i + 1];
            const pair = .{ .key = key, .value = value };
            try pairs.put(key.hashKey(), pair);
        }

        return .{ .hash = .{ .pairs = pairs } };
    }

    fn buildArray(self: *Self, startIndex: usize, endIndex: usize) !object.Object {
        var elements = ArrayList(*object.Object).init(self.arena.allocator());
        try elements.ensureTotalCapacity(endIndex - startIndex);
        for (startIndex..endIndex) |i| {
            const elem = try self.arena.allocator().create(object.Object);
            elem.* = self.stack[i];
            try elements.append(elem);
        }
        return object.Object{ .array = elements };
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
        } else if (left == .string and right == .string) {
            return try self.executeBinaryStringOperation(op, left, right);
        }

        return std.debug.panic("unsupported types for binary operation: {s} {s}", .{ right.typeId(), left.typeId() });
    }

    fn executeBinaryStringOperation(self: *Self, op: code.Constants, left: object.Object, right: object.Object) !void {
        if (op != .OpAdd) {
            return std.debug.panic("unknown string operator: {any}", .{op});
        }

        const leftVal = left.stringValue();
        const rightVal = right.stringValue();
        const result = std.fmt.allocPrint(self.arena.allocator(), "{s}{s}", .{ leftVal, rightVal }) catch return std.debug.panic("out of memory", .{});

        try self.push(object.Object{ .string = result });
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
const expectEqualSlices = std.testing.expectEqualSlices;
const expectEqual = std.testing.expectEqual;

const hashpair = struct {
    key: object.HashKey,
    value: i64,
};

const ExpectedValue = union(enum) {
    integer: i32,
    boolean: bool,
    string: []const u8,
    intArray: []const i64,
    hash: []const hashpair,
    err: []const u8,
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

test "test global let statements" {
    const tests = &[_]vmTestCase{
        .{ .input = "let one = 1; one", .expected = .{ .integer = 1 } },
        .{ .input = "let one = 1; let two = 2; one + two", .expected = .{ .integer = 3 } },
        .{ .input = "let one = 1; let two = one + one; one + two", .expected = .{ .integer = 3 } },
    };

    try runTests(tests);
}

test "test string expressions" {
    const tests = &[_]vmTestCase{
        .{ .input = "\"monkey\"", .expected = .{ .string = "monkey" } },
        .{ .input = "\"mon\" + \"key\"", .expected = .{ .string = "monkey" } },
        .{ .input = "\"mon\" + \"key\" + \"banana\"", .expected = .{ .string = "monkeybanana" } },
    };

    try runTests(tests);
}

test "test array literals" {
    const tests = &[_]vmTestCase{
        .{ .input = "[]", .expected = .{ .intArray = &[_]i64{} } },
        .{ .input = "[1, 2, 3]", .expected = .{ .intArray = &[_]i64{ 1, 2, 3 } } },
        .{ .input = "[1 + 2, 3 * 4, 5 + 6]", .expected = .{ .intArray = &[_]i64{ 3, 12, 11 } } },
    };

    try runTests(tests);
}

test "test hash literals" {
    const tests = &[_]vmTestCase{
        .{ .input = "{}", .expected = .{ .hash = &[_]hashpair{} } },
        .{
            .input = "{1: 2, 2: 3}",
            .expected = .{
                .hash = &[_]hashpair{
                    .{ .key = (object.Object{ .integer = 1 }).hashKey(), .value = 2 },
                    .{ .key = (object.Object{ .integer = 2 }).hashKey(), .value = 3 },
                },
            },
        },
        .{
            .input = "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
            .expected = .{
                .hash = &[_]hashpair{
                    .{ .key = (object.Object{ .integer = 2 }).hashKey(), .value = 4 },
                    .{ .key = (object.Object{ .integer = 6 }).hashKey(), .value = 16 },
                },
            },
        },
    };

    try runTests(tests);
}

test "test index expressions" {
    const tests = &[_]vmTestCase{
        .{ .input = "[1, 2, 3][1]", .expected = .{ .integer = 2 } },
        .{ .input = "[1, 2, 3][0 + 2]", .expected = .{ .integer = 3 } },
        .{ .input = "[[1, 1, 1]][0][0]", .expected = .{ .integer = 1 } },
        .{ .input = "[][0]", .expected = .nil },
        .{ .input = "[1, 2, 3][99]", .expected = .nil },
        .{ .input = "[1][-1]", .expected = .nil },
        .{ .input = "{1: 1, 2: 2}[1]", .expected = .{ .integer = 1 } },
        .{ .input = "{1: 1, 2: 2}[2]", .expected = .{ .integer = 2 } },
        .{ .input = "{1: 1}[0]", .expected = .nil },
        .{ .input = "{}[0]", .expected = .nil },
    };

    try runTests(tests);
}

test "test calling functions without arguments" {
    const tests = &[_]vmTestCase{
        .{ .input = "let fivePlusTen = fn() { 5 + 10; }; fivePlusTen();", .expected = .{ .integer = 15 } },
        .{ .input = "let one = fn() { 1; }; let two = fn() { 2; }; one() + two();", .expected = .{ .integer = 3 } },
        .{ .input = "let a = fn() { 1; }; let b = fn() { a() + 1; }; let c = fn() { b() + 1; }; c();", .expected = .{ .integer = 3 } },
    };

    try runTests(tests);
}

test "test functions with return statement" {
    const tests = &[_]vmTestCase{
        .{ .input = "let earlyExit = fn() { return 99; 100; }; earlyExit();", .expected = .{ .integer = 99 } },
        .{ .input = "let earlyExit = fn() { return 99; return 100; }; earlyExit();", .expected = .{ .integer = 99 } },
    };

    try runTests(tests);
}

test "test functions without return value" {
    const tests = &[_]vmTestCase{
        .{ .input = "let noReturn = fn() { }; noReturn();", .expected = .nil },
        .{ .input = "let noReturn = fn() { }; let noReturnTwo = fn() { noReturn(); }; noReturn(); noReturnTwo();", .expected = .nil },
    };

    try runTests(tests);
}

test "test first class functions" {
    const tests = &[_]vmTestCase{
        .{ .input = "let returnsOne = fn() { 1; }; let returnsOneReturner = fn() { returnsOne; }; returnsOneReturner()();", .expected = .{ .integer = 1 } },
        .{ .input = "let returnsOneReturner = fn() { let returnsOne = fn() { 1; }; returnsOne; }; returnsOneReturner()();", .expected = .{ .integer = 1 } },
    };

    try runTests(tests);
}

test "test calling functions with bindings" {
    const tests = &[_]vmTestCase{
        .{
            .input = "let one = fn() { let one = 1; one }; one();",
            .expected = .{ .integer = 1 },
        },
        .{
            .input = "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; oneAndTwo();",
            .expected = .{ .integer = 3 },
        },
        .{
            .input = "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; let threeAndFour = fn() { let three = 3; let four = 4; three + four; }; oneAndTwo() + threeAndFour();",
            .expected = .{ .integer = 10 },
        },
        .{
            .input = "let firstFoobar = fn() { let foobar = 50; foobar; }; let secondFoobar = fn() { let foobar = 100; foobar; }; firstFoobar() + secondFoobar();",
            .expected = .{ .integer = 150 },
        },
        .{
            .input = "let globalSeed = 50; let minusOne = fn() { let num = 1; globalSeed - num; }; let minusTwo = fn() { let num = 2; globalSeed - num; }; minusOne() + minusTwo();",
            .expected = .{ .integer = 97 },
        },
    };

    try runTests(tests);
}

test "test calling functions with arguments and bindings" {
    const tests = &[_]vmTestCase{
        .{
            .input = "let identity = fn(a) { a; }; identity(4);",
            .expected = .{ .integer = 4 },
        },
        .{
            .input = "let sum = fn(a, b) { a + b; }; sum(1, 2);",
            .expected = .{ .integer = 3 },
        },
        .{
            .input = "let sum = fn(a, b) { let c = a + b; c; }; sum(1, 2);",
            .expected = .{ .integer = 3 },
        },
        .{
            .input = "let sum = fn(a, b) { let c = a + b; c; }; sum(1, 2) + sum(3, 4);",
            .expected = .{ .integer = 10 },
        },
        .{
            .input = "let sum = fn(a, b) { let c = a + b; c; }; let outer = fn() { sum(1, 2) + sum(3, 4); }; outer();",
            .expected = .{ .integer = 10 },
        },
        .{
            .input = "let globalNum = 10; let sum = fn(a, b) { let c = a + b; c + globalNum; }; let outer = fn() { sum(1, 2) + sum(3, 4) + globalNum; }; outer() + globalNum;",
            .expected = .{ .integer = 50 },
        },
    };

    try runTests(tests);
}

test "test calling functions with wrong arguments" {
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    const tests = &[_]vmTestCase{
        .{ .input = "fn() { 1; }(1);", .expected = .{ .string = "wrong number of arguments: want=0, got=1" } },
        .{ .input = "fn(a) { a; }();", .expected = .{ .string = "wrong number of arugments: want=1, got=0" } },
        .{ .input = "fn(a, b) { a + b; }(1);", .expected = .{ .string = "wrong number of arguments: want=2, got=1" } },
        // .{ .input = "fn(a, b) { a + b; }(1, 2, 3);", .expected = .nil },
        // .{ .input = "fn(a, b) { a + b; }();", .expected = .nil },
    };

    for (tests) |tt| {
        // std.log.warn("input(vm.runTests): {c}", .{tt.input});
        var helper = parse(tt.input);
        defer helper.deinit();
        var comp = compiler.Compiler.init(test_allocator, definitions);
        try comp.compile(helper.node);
        defer comp.deinit();

        var vm = VM.init(test_allocator, comp.bytecode());
        defer vm.deinit();
        const res = vm.run();
        try std.testing.expectEqual(VMError.WrongNumberOfArguments, res);
        // const stackElem = vm.lastPoppedStackElem();
        // try testExpectedObject(tt.expected, stackElem);
    }

    // try runTests(tests);
}

test "test builtin functions" {
    const tests = &[_]vmTestCase{
        .{ .input = "len(\"\")", .expected = .{ .integer = 0 } },
        .{ .input = "len(\"four\")", .expected = .{ .integer = 4 } },
        .{ .input = "len(\"hello world\")", .expected = .{ .integer = 11 } },
        .{ .input = "len(1)", .expected = .{ .err = "argument to `len` not supported, got INTEGER" } },
        .{ .input = "len(\"one\", \"two\")", .expected = .{ .err = "wrong number of arguments. got=2, want=1" } },
        .{ .input = "len([1, 2, 3])", .expected = .{ .integer = 3 } },
        .{ .input = "len([])", .expected = .{ .integer = 0 } },
        .{ .input = "puts(\"hello\", \"world\")", .expected = .nil },
        .{ .input = "first([1, 2, 3])", .expected = .{ .integer = 1 } },
        .{ .input = "first([])", .expected = .nil },
        .{ .input = "first(1)", .expected = .{ .err = "argument to `first` must be ARRAY, got INTEGER" } },
        .{ .input = "last([1, 2, 3])", .expected = .{ .integer = 3 } },
        .{ .input = "last([])", .expected = .nil },
        .{ .input = "last(1)", .expected = .{ .err = "argument to `last` must be ARRAY, got INTEGER" } },
        .{ .input = "rest([1, 2, 3])", .expected = .{ .intArray = &[_]i64{ 2, 3 } } },
        .{ .input = "rest([])", .expected = .nil },
        .{ .input = "push([], 1)", .expected = .{ .intArray = &[_]i64{1} } },
        .{ .input = "push(1, 1)", .expected = .{ .err = "argument to `push` must be ARRAY, got INTEGER" } },
    };

    try runTests(tests);
}

test "test closures" {
    const tests = &[_]vmTestCase{
        .{
            .input = "let newClosure = fn(a) { fn() { a; }; }; let closure = newClosure(99); closure();",
            .expected = .{ .integer = 99 },
        },
        .{
            .input = "let newAdder = fn(a, b) { fn(c) { a + b + c; }; }; let adder = newAdder(1, 2); adder(8);",
            .expected = .{ .integer = 11 },
        },
        .{
            .input = "let newAdder = fn(a, b) { let c = a + b; fn(d) { c + d; }; }; let adder = newAdder(1, 2); adder(8);",
            .expected = .{ .integer = 11 },
        },
        .{
            .input = "let newAdderOuter = fn(a, b) { let c = a + b; fn(d) { let e = d + c; fn(f) { e + f; }; }; }; let newAdderInner = newAdderOuter(1, 2); let adder = newAdderInner(3); adder(8);",
            .expected = .{ .integer = 14 },
        },
        .{
            .input = "let a = 1; let newAdderOuter = fn(b) { fn(c) { fn(d) { a + b + c + d; }; }; }; let newAdderInner = newAdderOuter(2); let adder = newAdderInner(3); adder(8);",
            .expected = .{ .integer = 14 },
        },
        .{
            .input = "let newClosure = fn(a, b) { let one = fn() { a; }; let two = fn() { b; }; fn() { one() + two(); }; }; let closure = newClosure(9, 90); closure();",
            .expected = .{ .integer = 99 },
        },
    };

    try runTests(tests);
}

test "test recursive functions" {
    const tests = &[_]vmTestCase{
        .{
            .input = "let countDown = fn(x) { if (x == 0) { return 0; } else { countDown(x - 1); } }; countDown(1);",
            .expected = .{ .integer = 0 },
        },
        .{
            .input = "let countDown = fn(x) { if (x == 0) { return 0; } else { countDown(x - 1); } }; let wrapper = fn() { countDown(1); }; wrapper();",
            .expected = .{ .integer = 0 },
        },
        .{
            .input = "let wrapper = fn() { let countDown = fn(x) { if (x == 0) { return 0; } else { countDown(x - 1); } }; countDown(1); }; wrapper();",
            .expected = .{ .integer = 0 },
        },
        .{
            .input = "let wrapper = fn() { let countDown = fn(x) { if (x == 0) { return 0; } else { countDown(x - 1); } }; countDown(2); }; wrapper();",
            .expected = .{ .integer = 0 },
        },
    };

    try runTests(tests);
}

test "test recursive finbonacci" {
    const tests = &[_]vmTestCase{
        .{
            .input = "let fibonacci = fn(x) { if (x == 0) { return 0; } else { if (x == 1) { return 1; } else { fibonacci(x - 1) + fibonacci(x - 2); } } }; fibonacci(15);",
            .expected = .{ .integer = 610 },
        },
    };

    try runTests(tests);
}

pub fn runTests(tests: []const vmTestCase) !void {
    var definitions = try code.initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    for (tests) |tt| {
        // std.log.warn("input(vm.runTests): {s}", .{tt.input});
        var helper = parse(tt.input);
        defer helper.deinit();
        var comp = compiler.Compiler.init(test_allocator, definitions);
        try comp.compile(helper.node);
        defer comp.deinit();

        var vm = VM.init(test_allocator, comp.bytecode());
        defer vm.deinit();
        try vm.run();
        const stackElem = vm.lastPoppedStackElem();
        try testExpectedObject(tt.expected, stackElem);
    }
}

fn testExpectedObject(expected: ExpectedValue, actual: object.Object) !void {
    switch (expected) {
        .integer => {
            const i = actual.intValue();
            try expectEqual(i, expected.integer);
        },
        .boolean => {
            const b = actual.boolValue();
            try expectEqual(b, expected.boolean);
        },
        .string => {
            const s = actual.stringValue();
            try expectEqualSlices(u8, s, expected.string);
        },
        .intArray => {
            const elements = actual.array;
            assert(elements.items.len == expected.intArray.len);

            // try expectEqualSlices(i64, elements.items, expected.intArray);
            for (expected.intArray, 0..) |e, i| {
                const actualElem = elements.items[i];
                const actualInt = actualElem.intValue();
                try expectEqual(e, actualInt);
            }
        },
        .err => {
            const s = actual.err;
            try expectEqualSlices(u8, s, expected.err);
        },
        .hash => {
            const hash = actual.hash;
            assert(hash.pairs.count() == expected.hash.len);

            for (expected.hash) |p| {
                const expectedKey = p.key;
                const expectedValue = p.value;

                const pair = hash.pairs.get(expectedKey).?;

                const actualValue = pair.value.intValue();
                try expectEqual(expectedValue, actualValue);

                //     const actualPair = hash.pairs[i];
                //     const actualKey = actualPair.key;
                //     const actualValue = actualPair.value.intValue();

                //     try testExpectedObject(expectedKey, actualKey);
                //     try expectEqual(expectedValue, actualValue);
            }
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
