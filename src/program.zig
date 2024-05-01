const std = @import("std");
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const statement = @import("ast.zig");

pub const Program = struct {
    const Self = @This();

    statements: ArrayListUnmanaged(*statement.Statement),
    arena: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{ .statements = .{}, .arena = std.heap.ArenaAllocator.init(allocator) };
    }

    pub fn string(self: *Self) ![]u8 {
        var writer = std.mem.BufferWriter.init(self.allocator);
        try writer.print("Program {\n");
        try writer.print("  statements: ");
        try writer.print("{");
        try writer.print("\n");
        for (self.statements) |stmt| {
            try writer.print("    ");
            try writer.print("{");
            try writer.print("\n");
            try writer.print("      ");
            try writer.print(stmt.string());
            try writer.print("\n");
            try writer.print("    ");
            try writer.print("}");
            try writer.print("\n");
        }
        try writer.print("  ");
        try writer.print("}");
        try writer.print("\n");
        try writer.print("}");
        return writer.toOwnedSlice();
    }
};
