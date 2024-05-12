const std = @import("std");

pub const Instructions = []u8;

pub const Opcode = u8;

pub const Constants = enum(Opcode) {
    OpConstant = 0x00,
};

pub const Definition = struct {
    name: []const u8,
    operandWidths: []const usize,
};

pub var Definitions = std.AutoHashMapUnmanaged(Opcode, *Definition){};

pub fn initDefinitions(allocator: std.mem.Allocator) void {
    const opConstant = &Definition{
        .name = "OpConstant",
        .operandWidths = &[_]usize{2},
    };
    Definitions.put(allocator, @intFromEnum(Constants.OpConstant), @constCast(opConstant)) catch unreachable;
}

pub fn lookup(op: Opcode) ?*Definition {
    return Definitions.get(op);
}

pub fn make(allocator: std.mem.Allocator, op: Opcode, operands: []const i32) Instructions {
    // std.log.warn("make: {d}", .{op});
    const def = lookup(op);
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

    var offset: usize = 1;
    for (operands, 0..) |operand, i| {
        const width = def.?.operandWidths[i];
        // std.log.warn("operand: {any}", .{operand});
        // std.log.warn("  width: {any}", .{width});
        switch (width) {
            2 => {
                var buffer: [2]u8 = undefined;
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

    return instruction;
}

const assert = std.debug.assert;
const test_allocator = std.testing.allocator;
test "test make" {
    const tests = [_]struct {
        op: Opcode,
        operands: []const i32,
        expected: []const u8,
    }{
        .{ .op = @intFromEnum(Constants.OpConstant), .operands = &[_]i32{65534}, .expected = &[_]u8{ @intFromEnum(Constants.OpConstant), 255, 254 } },
    };

    initDefinitions(test_allocator);
    defer Definitions.deinit(test_allocator);

    for (tests) |tt| {
        const instruction = make(test_allocator, tt.op, tt.operands);
        defer test_allocator.free(instruction);
        // std.log.warn("instruction: {any}", .{instruction});
        // std.log.warn("expected: {any}", .{tt.expected});
        assert(instruction.len == tt.expected.len);

        for (tt.expected, 0..) |expected, i| {
            assert(instruction[i] == expected);
        }
    }
}
