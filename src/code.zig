const std = @import("std");
const utils = @import("utils.zig");

pub const Instructions = []const u8;

pub fn formatInstructions(allocator: std.mem.Allocator, definitions: Definitions, instructions: Instructions) ![]const u8 {
    var out = std.ArrayList(u8).init(allocator);

    var i: usize = 0;
    while (i < instructions.len) {
        const def = lookup(definitions, instructions[i]);
        if (def == null) {
            try out.writer().print("ERROR: undefined opcode: {d}", .{instructions[i]});
            continue;
        }

        const read = try readOperands(allocator, def.?.*, instructions[i + 1 ..]);
        defer allocator.free(read.operands);

        const f = try fmtInstruction(allocator, def.?.*, read.operands);
        defer allocator.free(f);
        try out.writer().print("{d:0>4} {s}\n", .{ i, f });

        i += 1 + read.offset;
    }

    return out.toOwnedSlice();
}

pub fn fmtInstruction(allocator: std.mem.Allocator, def: Definition, operands: []const usize) ![]const u8 {
    const operandCount: usize = def.operandWidths.len;
    if (operands.len != operandCount) {
        return try std.fmt.allocPrint(allocator, "ERROR: operand len {d} does not match expected {d}\n", .{ operands.len, operandCount });
    }

    switch (operandCount) {
        0 => {
            return try std.fmt.allocPrint(allocator, "{s}", .{def.name});
        },
        1 => {
            return try std.fmt.allocPrint(allocator, "{s} {d}", .{ def.name, operands[0] });
        },
        2 => {
            return try std.fmt.allocPrint(allocator, "{s} {d} {d}", .{ def.name, operands[0], operands[1] });
        },
        else => {},
    }

    return try std.fmt.allocPrint(allocator, "ERROR: unhandled operandCount for {s}\n", .{def.name});
}

pub const Opcode = u8;

pub const Constants = enum(u8) {
    OpConstant = 0x00,
    OpAdd = 0x01,
    OpPop = 0x02,
    OpSub = 0x03,
    OpMul = 0x04,
    OpDiv = 0x05,
    OpTrue = 0x06,
    OpFalse = 0x07,
    OpEqual = 0x08,
    OpNotEqual = 0x09,
    OpGreaterThan = 0x0a,
    OpMinus = 0x0b,
    OpBang = 0x0c,
    OpJumpNotTruthy = 0x0d,
    OpJump = 0x0e,
    OpNull = 0x0f,
    OpGetGlobal = 0x10,
    OpSetGlobal = 0x11,
    OpArray = 0x12,
    OpHash = 0x13,
    OpIndex = 0x14,
    OpCall = 0x15,
    OpReturnValue = 0x16,
    OpReturn = 0x17,
    OpGetLocal = 0x18,
    OpSetLocal = 0x19,
    OpGetBuiltin = 0x1a,
    OpClosure = 0x1b,
    OpGetFree = 0x1c,
    OpCurrentClosure = 0x1d,
};

pub const Definition = struct {
    name: []const u8,
    operandWidths: []const usize,
};

pub const ReadOperand = struct {
    operands: []usize,
    offset: usize,
};

pub const Definitions = std.AutoHashMapUnmanaged(Opcode, *const Definition);

pub fn initDefinitions(allocator: std.mem.Allocator) !Definitions {
    var definitions = std.AutoHashMapUnmanaged(u8, *const Definition){};
    const opConstant = &Definition{
        .name = "OpConstant",
        .operandWidths = &[_]usize{2},
    };

    const opAdd = &Definition{
        .name = "OpAdd",
        .operandWidths = &[_]usize{},
    };

    const opPop = &Definition{
        .name = "OpPop",
        .operandWidths = &[_]usize{},
    };

    const opSub = &Definition{
        .name = "OpSub",
        .operandWidths = &[_]usize{},
    };

    const opMul = &Definition{
        .name = "OpMul",
        .operandWidths = &[_]usize{},
    };

    const opDiv = &Definition{
        .name = "OpDiv",
        .operandWidths = &[_]usize{},
    };

    const opTrue = &Definition{
        .name = "OpTrue",
        .operandWidths = &[_]usize{},
    };

    const opFalse = &Definition{
        .name = "OpFalse",
        .operandWidths = &[_]usize{},
    };

    const opEqual = &Definition{
        .name = "OpEqual",
        .operandWidths = &[_]usize{},
    };

    const opNotEqual = &Definition{
        .name = "OpNotEqual",
        .operandWidths = &[_]usize{},
    };

    const opGreaterThan = &Definition{
        .name = "OpGreaterThan",
        .operandWidths = &[_]usize{},
    };

    const opMinus = &Definition{
        .name = "OpMinus",
        .operandWidths = &[_]usize{},
    };

    const opBang = &Definition{
        .name = "OpBang",
        .operandWidths = &[_]usize{},
    };

    const opJumpNotTruthy = &Definition{
        .name = "OpJumpNotTruthy",
        .operandWidths = &[_]usize{2},
    };

    const opJump = &Definition{
        .name = "OpJump",
        .operandWidths = &[_]usize{2},
    };

    const opNull = &Definition{
        .name = "OpNull",
        .operandWidths = &[_]usize{},
    };

    const opGetGlobal = &Definition{
        .name = "OpGetGlobal",
        .operandWidths = &[_]usize{2},
    };

    const opSetGlobal = &Definition{
        .name = "OpSetGlobal",
        .operandWidths = &[_]usize{2},
    };

    const opArray = &Definition{
        .name = "OpArray",
        .operandWidths = &[_]usize{2},
    };

    const opHash = &Definition{
        .name = "OpHash",
        .operandWidths = &[_]usize{2},
    };

    const opIndex = &Definition{
        .name = "OpIndex",
        .operandWidths = &[_]usize{},
    };

    const opCall = &Definition{
        .name = "OpCall",
        .operandWidths = &[_]usize{1},
    };

    const opReturnValue = &Definition{
        .name = "OpReturnValue",
        .operandWidths = &[_]usize{},
    };

    const opReturn = &Definition{
        .name = "OpReturn",
        .operandWidths = &[_]usize{},
    };

    const opGetLocal = &Definition{
        .name = "OpGetLocal",
        .operandWidths = &[_]usize{1},
    };

    const opSetLocal = &Definition{
        .name = "OpSetLocal",
        .operandWidths = &[_]usize{1},
    };

    const opGetBuiltin = &Definition{
        .name = "OpGetBuiltin",
        .operandWidths = &[_]usize{1},
    };

    const opClosure = &Definition{
        .name = "OpClosure",
        .operandWidths = &[_]usize{ 2, 1 },
    };

    const opGetFree = &Definition{
        .name = "OpGetFree",
        .operandWidths = &[_]usize{1},
    };

    const opCurrentClosure = &Definition{
        .name = "OpCurrentClosure",
        .operandWidths = &[_]usize{},
    };

    // std.log.warn("Bit size: {d}", .{@bitSizeOf(@TypeOf(@intFromEnum(Constants.OpConstant)))});
    // Definitions.put(allocator, @intFromEnum(Constants.OpConstant), opConstant) catch unreachable;
    try definitions.put(allocator, @intFromEnum(Constants.OpConstant), opConstant);
    try definitions.put(allocator, @intFromEnum(Constants.OpAdd), opAdd);
    try definitions.put(allocator, @intFromEnum(Constants.OpPop), opPop);
    try definitions.put(allocator, @intFromEnum(Constants.OpSub), opSub);
    try definitions.put(allocator, @intFromEnum(Constants.OpMul), opMul);
    try definitions.put(allocator, @intFromEnum(Constants.OpDiv), opDiv);
    try definitions.put(allocator, @intFromEnum(Constants.OpTrue), opTrue);
    try definitions.put(allocator, @intFromEnum(Constants.OpFalse), opFalse);
    try definitions.put(allocator, @intFromEnum(Constants.OpEqual), opEqual);
    try definitions.put(allocator, @intFromEnum(Constants.OpNotEqual), opNotEqual);
    try definitions.put(allocator, @intFromEnum(Constants.OpGreaterThan), opGreaterThan);
    try definitions.put(allocator, @intFromEnum(Constants.OpMinus), opMinus);
    try definitions.put(allocator, @intFromEnum(Constants.OpBang), opBang);
    try definitions.put(allocator, @intFromEnum(Constants.OpJumpNotTruthy), opJumpNotTruthy);
    try definitions.put(allocator, @intFromEnum(Constants.OpJump), opJump);
    try definitions.put(allocator, @intFromEnum(Constants.OpNull), opNull);
    try definitions.put(allocator, @intFromEnum(Constants.OpGetGlobal), opGetGlobal);
    try definitions.put(allocator, @intFromEnum(Constants.OpSetGlobal), opSetGlobal);
    try definitions.put(allocator, @intFromEnum(Constants.OpArray), opArray);
    try definitions.put(allocator, @intFromEnum(Constants.OpHash), opHash);
    try definitions.put(allocator, @intFromEnum(Constants.OpIndex), opIndex);
    try definitions.put(allocator, @intFromEnum(Constants.OpCall), opCall);
    try definitions.put(allocator, @intFromEnum(Constants.OpReturnValue), opReturnValue);
    try definitions.put(allocator, @intFromEnum(Constants.OpReturn), opReturn);
    try definitions.put(allocator, @intFromEnum(Constants.OpGetLocal), opGetLocal);
    try definitions.put(allocator, @intFromEnum(Constants.OpSetLocal), opSetLocal);
    try definitions.put(allocator, @intFromEnum(Constants.OpGetBuiltin), opGetBuiltin);
    try definitions.put(allocator, @intFromEnum(Constants.OpClosure), opClosure);
    try definitions.put(allocator, @intFromEnum(Constants.OpGetFree), opGetFree);
    try definitions.put(allocator, @intFromEnum(Constants.OpCurrentClosure), opCurrentClosure);

    return definitions;
}

pub fn lookup(definitions: std.AutoHashMapUnmanaged(u8, *const Definition), op: Opcode) ?*const Definition {
    return definitions.get(op);
}

pub fn readOperands(allocator: std.mem.Allocator, def: Definition, instructions: Instructions) !ReadOperand {
    var operands: []usize = try allocator.alloc(usize, def.operandWidths.len);
    var offset: usize = 0;

    for (def.operandWidths, 0..) |width, i| {
        switch (width) {
            2 => {
                // const as_pointer_to_array: *[2]u8 = &instructions;
                var mem: [2]u8 = undefined;
                for (instructions[offset .. offset + 2], 0..) |byte, j| {
                    mem[j] = byte;
                }
                const operand = std.mem.readInt(u16, &mem, .big);
                operands[i] = operand;
            },
            1 => {
                operands[i] = @intCast(instructions[offset]);
            },
            else => {
                std.debug.panic("Unhandled operand width", .{});
            },
        }

        offset += width;
    }

    return .{ .operands = operands, .offset = offset };
}

pub fn make(allocator: std.mem.Allocator, definitions: std.AutoHashMapUnmanaged(u8, *const Definition), op: Opcode, operands: []const usize) Instructions {
    // std.log.warn("make: {d}", .{op});
    const def = lookup(definitions, op);
    if (def == null) {
        // std.log.warn("opcode {d} has no associated definition", .{op});
        return &[_]u8{};
    }

    var instructionLen: usize = 1;
    for (def.?.operandWidths) |width| {
        instructionLen += width;
    }

    const instruction = allocator.alloc(u8, instructionLen) catch unreachable;
    @memset(instruction, 0);
    instruction[0] = op;

    // std.log.warn("instructionLen: {d}", .{instructionLen});
    // std.log.warn("instruction (before loop): {any}", .{instruction});

    var offset: usize = 1;
    for (operands, 0..) |operand, i| {
        const width = def.?.operandWidths[i];
        // std.log.warn("operand: {any}", .{operand});
        // std.log.warn("  width: {any}", .{width});
        switch (width) {
            2 => {
                var buffer: [2]u8 = undefined;
                // std.log.warn("operand: {d}", .{operand});
                std.mem.writeInt(u16, &buffer, @intCast(operand), .big);
                // std.log.warn("buffer: {any}", .{buffer});
                for (buffer, 0..) |byte, j| {
                    const idx = offset + j;
                    // std.log.warn("idx: {d}", .{idx});
                    instruction[idx] = byte;
                }
            },
            1 => {
                instruction[offset] = @intCast(operand);
            },
            else => {
                std.debug.panic("Unhandled operand width", .{});
            },
        }

        offset += width;
    }

    // std.log.warn("ins (make): {any}", .{instruction});

    return instruction;
}

const assert = std.debug.assert;
const test_allocator = std.testing.allocator;
test "test make" {
    // std.log.warn("test make", .{});
    const tests = [_]struct {
        op: Opcode,
        operands: []const usize,
        expected: []const u8,
    }{
        .{
            .op = @intFromEnum(Constants.OpConstant),
            .operands = &[_]usize{65534},
            .expected = &[_]u8{ @intFromEnum(Constants.OpConstant), 255, 254 },
        },
        .{
            .op = @intFromEnum(Constants.OpAdd),
            .operands = &[_]usize{},
            .expected = &[_]u8{@intFromEnum(Constants.OpAdd)},
        },
        .{
            .op = @intFromEnum(Constants.OpGetLocal),
            .operands = &[_]usize{255},
            .expected = &[_]u8{ @intFromEnum(Constants.OpGetLocal), 255 },
        },
        .{
            .op = @intFromEnum(Constants.OpClosure),
            .operands = &[_]usize{ 65534, 255 },
            .expected = &[_]u8{ @intFromEnum(Constants.OpClosure), 255, 254, 255 },
        },
    };

    var definitions = try initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    for (tests) |tt| {
        const instruction = make(test_allocator, definitions, tt.op, tt.operands);
        defer test_allocator.free(instruction);
        // std.log.warn("instruction: {any}", .{instruction});
        // std.log.warn("expected: {any}", .{tt.expected});
        try std.testing.expectEqual(instruction.len, tt.expected.len);

        for (tt.expected, 0..) |expected, i| {
            try std.testing.expectEqual(instruction[i], expected);
        }
    }
}

test "test instruction string" {
    var definitions = try initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    const instructions = [_]Instructions{
        make(test_allocator, definitions, @intFromEnum(Constants.OpAdd), &[_]usize{}),
        make(test_allocator, definitions, @intFromEnum(Constants.OpGetLocal), &[_]usize{1}),
        make(test_allocator, definitions, @intFromEnum(Constants.OpConstant), &[_]usize{2}),
        make(test_allocator, definitions, @intFromEnum(Constants.OpConstant), &[_]usize{65535}),
        make(test_allocator, definitions, @intFromEnum(Constants.OpClosure), &[_]usize{ 65535, 255 }),
    };

    const expected: []const u8 =
        \\0000 OpAdd
        \\0001 OpGetLocal 1
        \\0003 OpConstant 2
        \\0006 OpConstant 65535
        \\0009 OpClosure 65535 255
        \\
    ;

    const contatted = try utils.flatten(test_allocator, &instructions);
    defer test_allocator.free(contatted);

    for (instructions) |ins| {
        test_allocator.free(ins);
    }

    const actual = try formatInstructions(test_allocator, definitions, contatted);
    defer test_allocator.free(actual);
    // std.log.warn("{s}", .{expected});
    // std.log.warn("{s}", .{actual});
    assert(std.mem.eql(u8, actual, expected));
}

test "test read operands" {
    const tests = [_]struct { op: Opcode, operands: []const usize, bytesRead: usize }{
        .{ .op = @intFromEnum(Constants.OpConstant), .operands = &[_]usize{65535}, .bytesRead = 2 },
        .{ .op = @intFromEnum(Constants.OpGetLocal), .operands = &[_]usize{255}, .bytesRead = 1 },
        .{ .op = @intFromEnum(Constants.OpClosure), .operands = &[_]usize{ 65535, 255 }, .bytesRead = 3 },
    };

    var definitions = try initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    for (tests) |tt| {
        const instruction = make(test_allocator, definitions, tt.op, tt.operands);
        defer test_allocator.free(instruction);

        const def = lookup(definitions, tt.op);
        assert(def != null);

        const read = try readOperands(test_allocator, def.?.*, instruction);
        defer test_allocator.free(read.operands);
        assert(read.offset == tt.bytesRead);
    }
}
