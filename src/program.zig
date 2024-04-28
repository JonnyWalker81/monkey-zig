const std = @import("std");
const ArrayList = std.ArrayList;
const statement = @import("ast.zig");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

pub const Program = struct {
    statements: ArrayList(statement.Statement),

    pub fn init() !Program {
        return .{ .statements = ArrayList(statement.Statement).init(allocator) };
    }
};
