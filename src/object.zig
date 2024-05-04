const std = @import("std");

pub const Object = union(enum) {
    const Self = @This();

    nil,
    integer: i64,
    boolean: bool,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .nil => try writer.print("null", .{}),
            .integer => |i| {
                try writer.print("{d}", .{i});
            },
            .boolean => |b| {
                try writer.print("{}", .{b});
            },
        }
    }
};
