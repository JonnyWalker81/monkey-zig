const std = @import("std");
const ArrayList = std.ArrayList;
const statement = @import("ast.zig");

pub const Program = struct {
    const Self = @This();

    statements: ArrayList(*statement.Statement),
    arena: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{ .statements = ArrayList(*statement.Statement).init(allocator), .arena = std.heap.ArenaAllocator.init(allocator) };
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        // try writer.print("{s}", .{self.string()});

        // try writer.print("Program {{\n", .{});
        // try writer.print("  statements: ", .{});
        // try writer.print("{{", .{});
        // try writer.print("\n", .{});
        for (self.statements.items) |stmt| {
            // try writer.print("    ", .{});
            // try writer.print("{{", .{});
            // try writer.print("\n", .{});
            // try writer.print("      ", .{});
            try writer.print("{s}", .{stmt});
            // try writer.print("\n", .{});
            // try writer.print("    ", .{});
            // try writer.print("}}", .{});
            // try writer.print("\n", .{});
        }
        // try writer.print("  ", .{});
        // try writer.print("}}", .{});
        // try writer.print("\n", .{});
        // try writer.print("}}", .{});
    }

    // pub fn string(self: Self) ![]u8 {
    //     var writer = std.mem.BufferWriter.init(self.allocator);
    //     try writer.print("Program {\n");
    //     try writer.print("  statements: ");
    //     try writer.print("{");
    //     try writer.print("\n");
    //     for (self.statements) |stmt| {
    //         try writer.print("    ");
    //         try writer.print("{");
    //         try writer.print("\n");
    //         try writer.print("      ");
    //         try writer.print(stmt.string());
    //         try writer.print("\n");
    //         try writer.print("    ");
    //         try writer.print("}");
    //         try writer.print("\n");
    //     }
    //     try writer.print("  ");
    //     try writer.print("}");
    //     try writer.print("\n");
    //     try writer.print("}");
    //     return writer.toOwnedSlice();
    // }
};
