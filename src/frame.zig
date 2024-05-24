const std = @import("std");
const object = @import("object.zig");
const code = @import("code.zig");

pub const Frame = struct {
    const Self = @This();

    func: object.Object,
    ip: i32,

    pub fn init(func: object.Object) Frame {
        return Frame{ .func = func, .ip = -1 };
    }

    pub fn instructions(self: *Self) code.Instructions {
        return self.func.instructions;
    }
};
