const std = @import("std");
const token = @import("token.zig");

pub const Expression = union(enum) {
    const Self = @This();

    empty: void,
    identifier: Identifier,
    integer: i64,
    prefix: struct {
        operator: []const u8,
        right: *Expression,
    },
    infix: struct {
        left: *Expression,
        operator: []const u8,
        right: *Expression,
    },
    boolean: bool,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;

        switch (self) {
            .empty => {},
            .identifier => |ident| {
                try writer.print("{s}", .{ident.identifier});
            },
            .integer => |int| {
                try writer.print("{d}", .{int});
            },
            .prefix => |prefix| {
                try writer.print("(", .{});
                try writer.print("{s}", .{prefix.operator});
                try prefix.right.*.format("{s}", options, writer);
                try writer.print(")", .{});
            },
            .infix => |infix| {
                try writer.print("(", .{});
                try infix.left.*.format("{s}", options, writer);
                try writer.print(" {s} ", .{infix.operator});
                try infix.right.*.format("{s}", options, writer);
                try writer.print(")", .{});
            },
            .boolean => |boolean| {
                try writer.print("{}", .{boolean});
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

    empty: void,

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
            .empty => {},
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
                // try writer.writeAll(";");
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
