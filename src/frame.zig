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
        switch (self.func) {
            .compiledFunction => |f| {
                return f.instructions;
            },
            else => {
                std.debug.print("Unsupported object type: {any}\n", .{self.func});
                std.process.exit(1);
            },
        }
    }
};
