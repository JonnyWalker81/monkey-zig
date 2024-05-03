const std = @import("std");
const token = @import("token.zig");
const ArrayListUnmanaged = std.ArrayListUnmanaged;

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
    ifExpression: struct {
        condition: *Expression,
        consequence: *BlockStatement,
        alternative: ?*BlockStatement,
    },

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .empty => {
                try writer.print("", .{});
            },
            .identifier => |ident| {
                try writer.print("{s}", .{ident.identifier});
            },
            .integer => |int| {
                try writer.print("{d}", .{int});
            },
            .prefix => |prefix| {
                try writer.print("(", .{});
                try writer.print("{s}", .{prefix.operator});
                try writer.print("{s}", .{prefix.right});
                try writer.print(")", .{});
            },
            .infix => |infix| {
                try writer.print("(", .{});
                try writer.print("{s} ", .{infix.left});
                try writer.print("{s} ", .{infix.operator});
                try writer.print("{s}", .{infix.right});
                try writer.print(")", .{});
            },
            .boolean => |boolean| {
                try writer.print("{}", .{boolean});
            },
            .ifExpression => |ifExpr| {
                // try writer.print("if (", .{});
                // try ifExpr.condition.*.format("{s}", options, writer);
                // try writer.print(") ", .{});
                // try ifExpr.consequence.format("{s}", options, writer);
                if (ifExpr.alternative) |alternative| {
                    try writer.print(" else ", .{});
                    try writer.print("{s}", .{alternative});
                }
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
        _ = options;

        switch (self) {
            .empty => {},
            .letStatement => |ls| {
                try writer.print("let {s} = ", .{ls.identifier.identifier});
                try writer.print("{s}", .{ls.expression});
                try writer.writeAll(";");
            },
            .returnStatement => |rs| {
                try writer.writeAll("return ");
                try writer.print("{s}", .{rs.expression});
                try writer.writeAll(";");
            },
            .expressionStatement => |es| {
                try writer.print("{s}", .{es.expression});
                // try writer.writeAll(";");
            },
        }
    }
};

pub const BlockStatement = struct {
    const Self = @This();

    statements: ArrayListUnmanaged(*Statement),

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        // _ = self;

        try writer.writeAll("{");
        for (self.statements.items) |statement| {
            try writer.print("{s}", .{statement});
        }
        try writer.writeAll("}");
    }
};

pub const Identifier = struct {
    identifier: []const u8,

    pub fn init(ident: []const u8) Identifier {
        return .{ .identifier = ident };
    }
};
