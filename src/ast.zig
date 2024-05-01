const std = @import("std");
const token = @import("token.zig");

pub const Expression = union(enum) {
    const Self = @This();

    identifier: Identifier,
    integer: i64,
    prefix: struct {
        operator: []const u8,
        right: ?*Expression,
    },

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;

        switch (self) {
            .identifier => |ident| {
                try writer.print("{s}", .{ident.identifier});
            },
            .integer => |int| {
                try writer.print("{d}", .{int});
            },
            .prefix => |prefix| {
                try writer.print("{s}", .{prefix.operator});
                try prefix.right.?.format("{s}", options, writer);
            },
        }
    }
};

// pub const LetStatement = struct {
//     identifier: Identifier,
//     expression: Expression,
// };

pub const Statement = union(enum) {
    const Self = @This();

    letStatement: struct {
        identifier: Identifier,
        expression: Expression,
    },
    returnStatement: struct {
        expression: Expression,
    },
    expressionStatement: struct {
        expression: *Expression,
    },

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;

        switch (self) {
            .letStatement => |ls| {
                try writer.print("let {s} = ", .{ls.identifier.identifier});
                try ls.expression.format("{s}", options, writer);
                try writer.writeAll(";");
            },
            .returnStatement => |rs| {
                try writer.writeAll("return ");
                try rs.expression.format("{s}", options, writer);
                try writer.writeAll(";");
            },
            .expressionStatement => |es| {
                try es.expression.format("{s}", options, writer);
                try writer.writeAll(";");
            },
        }
    }
};

pub const Identifier = struct {
    identifier: []const u8,

    pub fn init(ident: []const u8) Identifier {
        return .{ .identifier = ident };
    }
};
