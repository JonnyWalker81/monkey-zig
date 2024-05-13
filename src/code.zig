const std = @import("std");
const utils = @import("utils.zig");

pub const Instructions = []const u8;

pub fn format(
    self: Instructions,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = self;
    _ = fmt;
    _ = options;
    _ = writer;
}

pub const Opcode = u8;

pub const Constants = enum(u8) {
    OpConstant = 0x00,
};

pub const Definition = struct {
    name: []const u8,
    operandWidths: []const usize,
};

pub const ReadOperand = struct {
    operands: []usize,
    offset: usize,
};

// pub var Definitions = std.AutoHashMapUnmanaged(Opcode, *const Definition){};
// pub var Definitions = std.AutoHashMapUnmanaged(u8, *const Definition){};

pub fn initDefinitions(allocator: std.mem.Allocator) std.AutoHashMapUnmanaged(u8, *const Definition) {
    var Definitions = std.AutoHashMapUnmanaged(u8, *const Definition){};
    const opConstant = &Definition{
        .name = "OpConstant",
        .operandWidths = &[_]usize{2},
    };
    std.log.warn("Bit size: {d}", .{@bitSizeOf(@TypeOf(@intFromEnum(Constants.OpConstant)))});
    // Definitions.put(allocator, @intFromEnum(Constants.OpConstant), opConstant) catch unreachable;
    Definitions.put(allocator, 0, opConstant) catch unreachable;

    return Definitions;
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
    const def = lookup(definitions, 0);
    if (def == null) {
        std.log.warn("opcode {d} has no associated definition", .{op});
        return &[_]u8{};
    }

    var instructionLen: usize = 1;
    for (def.?.operandWidths) |width| {
        instructionLen += width;
    }

    const instruction = allocator.alloc(u8, instructionLen) catch unreachable;
    instruction[0] = op;

    std.log.warn("instructionLen: {d}", .{instructionLen});

    var offset: usize = 1;
    for (operands, 0..) |operand, i| {
        const width = def.?.operandWidths[i];
        // std.log.warn("operand: {any}", .{operand});
        // std.log.warn("  width: {any}", .{width});
        switch (width) {
            2 => {
                var buffer: [2]u8 = undefined;
                std.log.warn("operand: {d}", .{operand});
                std.mem.writeInt(u16, &buffer, @intCast(operand), .big);
                for (buffer, 0..) |byte, j| {
                    instruction[offset + j] = byte;
                }
            },
            else => {
                std.debug.panic("Unhandled operand width", .{});
            },
        }

        offset += width;
    }

    std.log.warn("ins: {any}", .{instruction});

    return instruction;
}

const assert = std.debug.assert;
const test_allocator = std.testing.allocator;
test "test make" {
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
    };

    var definitions = initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    for (tests) |tt| {
        const instruction = make(test_allocator, definitions, tt.op, tt.operands);
        defer test_allocator.free(instruction);
        // std.log.warn("instruction: {any}", .{instruction});
        // std.log.warn("expected: {any}", .{tt.expected});
        assert(instruction.len == tt.expected.len);

        for (tt.expected, 0..) |expected, i| {
            assert(instruction[i] == expected);
        }
    }
}

test "test instruction string" {
    var definitions = initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    const instructions = [_]Instructions{
        make(test_allocator, definitions, @intFromEnum(Constants.OpConstant), &[_]usize{1}),
        make(test_allocator, definitions, @intFromEnum(Constants.OpConstant), &[_]usize{2}),
        make(test_allocator, definitions, @intFromEnum(Constants.OpConstant), &[_]usize{65535}),
    };

    const expected: []const u8 =
        \\"0000 OpConstant 1",
        \\"0003 OpConstant 2",
        \\"0006 OpConstant 65535",
    ;

    const contatted = try utils.flatten(test_allocator, &instructions);
    defer test_allocator.free(contatted);

    const actual = try std.fmt.allocPrint(test_allocator, "{s}", .{contatted});
    assert(std.mem.eql(u8, actual, expected));
}

test "test read operands" {
    const tests = [_]struct { op: Opcode, operands: []const usize, bytesRead: usize }{
        .{ .op = @intFromEnum(Constants.OpConstant), .operands = &[_]usize{65535}, .bytesRead = 2 },
    };

    var definitions = initDefinitions(test_allocator);
    defer definitions.deinit(test_allocator);

    for (tests) |tt| {
        const instruction = make(test_allocator, definitions, tt.op, tt.operands);
        defer test_allocator.free(instruction);

        const def = lookup(definitions, tt.op);
        if (def == null) {
            std.debug.panic("Definition not found", .{});
        }

        const read = try readOperands(test_allocator, def.?.*, instruction);
        assert(read.offset == tt.bytesRead);
    }
}
