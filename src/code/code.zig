const std = @import("std");

const Instructions = []u8;

const Opcode = u8;

const Constants = enum(Opcode) {
    OpConstant = 0x01,
};

const Definition = struct {
    name: []const u8,
    operandWidths: []i32,
};

const Definitions = std.AutoHashMapUnmanaged(Opcode, *Definition){};

pub fn initDefinitions(allocator: std.mem.Allocator) void {
    Definitions.put(allocator, Constants.OpConstant, &Definition{
        .name = "OpConstant",
        .operandWidths = []i32{2},
    });
}

pub fn lookup(op: Opcode) ?*Definition {
    return Definitions.get(op);
}

test "test make" {}
