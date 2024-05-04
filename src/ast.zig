const std = @import("std");
const token = @import("token.zig");
const program = @import("program.zig");
const ArrayListUnmanaged = std.ArrayListUnmanaged;

pub const Node = union(enum) {
    const Self = @This();

    statement: *Statement,
    expression: *Expression,
    program: *program.Program,
};

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
    functionLiteral: struct {
        parameters: ArrayListUnmanaged(*Identifier),
        body: *BlockStatement,
    },
    callExpression: struct {
        function: *Expression,
        arguments: ArrayListUnmanaged(*Expression),
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
                try writer.print("if (", .{});
                try writer.print("{s}", .{ifExpr.condition});
                try writer.print(") ", .{});
                try writer.print("{s}", .{ifExpr.consequence});
                if (ifExpr.alternative) |alternative| {
                    try writer.print("else ", .{});
                    try writer.print("{s}", .{alternative});
                }
            },
            .functionLiteral => |fnLit| {
                try writer.print("fn(", .{});
                for (fnLit.parameters.items) |param| {
                    try writer.print("{s}", .{param.identifier});
                }
                try writer.print(") ", .{});
                try writer.print("{s}", .{fnLit.body});
            },
            .callExpression => |call| {
                try writer.print("{s}(", .{call.function});
                for (call.arguments.items, 0..) |arg, i| {
                    if (i > 0) {
                        try writer.print(", ", .{});
                    }
                    try writer.print("{s}", .{arg});
                }
                try writer.print(")", .{});
            },
        }
    }
};

pub const Statement = union(enum) {
    const Self = @This();

    empty,

    letStatement: struct {
        identifier: Identifier,
        expression: *Expression,
    },
    returnStatement: struct {
        expression: *Expression,
    },
    expressionStatement: struct {
        expression: *Expression,
    },

    pub fn init(allocator: std.mem.Allocator) *Statement {
        var statement = allocator.create(Statement) catch {
            std.debug.panic("Failed to allocate Statement", .{});
        };

        return statement;
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
            },
        }
    }
};

pub const BlockStatement = struct {
    const Self = @This();

    statements: ArrayListUnmanaged(*Statement),

    pub fn init(allocator: std.mem.Allocator) *BlockStatement {
        var block = allocator.create(BlockStatement) catch {
            std.debug.panic("Failed to allocate BlockStatement", .{});
        };

        block.statements = .{};
        return block;
    }

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
