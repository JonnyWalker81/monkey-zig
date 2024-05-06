const std = @import("std");
const ast = @import("ast.zig");
const environment = @import("environment.zig");
const ArrayListUnmanaged = std.ArrayListUnmanaged;

pub const Object = union(enum) {
    const Self = @This();
    // allocator: std.mem.Allocator,

    nil,
    integer: i64,
    boolean: bool,
    returnValue: *Self,
    function: struct {
        parameters: ArrayListUnmanaged(*ast.Identifier),
        body: *ast.BlockStatement,
        env: *environment.Environment,
    },
    err: []const u8,

    // pub fn init(allocator: std.mem.Allocator) Self {
    //     return .{ .allocator = allocator };
    // }

    // pub fn initWithFunction(
    //     allocator: std.mem.Allocator,
    //     parameters: ArrayList(*ast.Identifier),
    //     body: *ast.BlockStatement,
    //     env: *environment.Environment,
    // ) Self {
    //     return .{
    //         .allocator = allocator,
    //         .function = .{
    //             .parameters = parameters.clone(allocator),
    //             .body = body,
    //             .env = env,
    //         },
    //     };
    // }

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
            .function => "FUNCTION",
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
            .function => |f| {
                try writer.print("fn", .{});
                try writer.print("(", .{});
                for (f.parameters.items, 0..) |param, i| {
                    try writer.print("{s}", .{param.identifier});
                    if (i != f.parameters.items.len - 1) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print(") ", .{});
                try writer.print("{s}", .{f.body});
                try writer.print("\n}}", .{});
            },
            .err => |e| {
                try writer.print("{s}", .{e});
            },
        }
    }
};
