const std = @import("std");

pub const Object = union(enum) {
    const Self = @This();

    nil,
    integer: i64,
    boolean: bool,
    returnValue: *Self,
    err: []const u8,

    pub fn intValue(self: *Self) i64 {
        return switch (self.*) {
            .integer => |i| i,
            else => -1,
        };
    }

    pub fn boolValue(self: *Self) bool {
        return switch (self.*) {
            .boolean => |b| b,
            .integer => |_| true,
            else => false,
        };
    }

    pub fn typeId(self: *Self) []const u8 {
        return switch (self.*) {
            .nil => "NULL",
            .integer => "INTEGER",
            .boolean => "BOOLEAN",
            .err => "ERROR",
            .returnValue => |r| r.typeId(),
        };
    }

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
            .returnValue => |r| {
                try writer.print("{s}", .{r});
            },
            .err => |e| {
                try writer.print("{s}", .{e});
            },
        }
    }
};
