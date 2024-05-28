const std = @import("std");
const object = @import("object.zig");
const code = @import("code.zig");

pub const Frame = struct {
    const Self = @This();

    cl: object.Object,
    ip: i32,
    basePointer: i32 = 0,

    pub fn init(cl: object.Object, basePointer: i32) Frame {
        return Frame{ .cl = cl, .ip = -1, .basePointer = basePointer };
    }

    pub fn instructions(self: *Self) code.Instructions {
        switch (self.cl) {
            .closure => |c| {
                return c.func.instructions;
            },
            else => {
                std.debug.print("Unsupported object type: {any}\n", .{self.cl});
                std.process.exit(1);
            },
        }
    }
};
