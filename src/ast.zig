const std = @import("std");
const token = @import("token.zig");
const program = @import("program.zig");
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

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
        parameters: ArrayList(*Identifier),
        body: *BlockStatement,
    },
    callExpression: struct {
        function: *Expression,
        arguments: ArrayList(*Expression),
    },
    stringLiteral: []const u8,
    arrayLiteral: struct {
        elements: ArrayList(*Expression),
    },
    indexExpression: struct {
        left: *Expression,
        index: *Expression,
    },
    hashLiteral: struct {
        pairs: AutoHashMap(*Expression, *Expression),
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
                for (fnLit.parameters.items, 0..) |param, i| {
                    if (i > 0) {
                        try writer.print(", ", .{});
                    }

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
            .stringLiteral => |str| {
                try writer.print("{s}", .{str});
            },
            .arrayLiteral => |arr| {
                try writer.print("[", .{});
                for (arr.elements.items, 0..) |elem, i| {
                    if (i > 0) {
                        try writer.print(", ", .{});
                    }
                    try writer.print("{s}", .{elem});
                }
                try writer.print("]", .{});
            },
            .indexExpression => |index| {
                try writer.print("({s}[{s}])", .{ index.left, index.index });
            },
            .hashLiteral => |hash| {
                try writer.print("{{", .{});
                var it = hash.pairs.iterator();
                var i: usize = 0;
                while (it.next()) |entry| {
                    if (i > 0) {
                        try writer.print(", ", .{});
                    }
                    try writer.print("{s}: {s}", .{ entry.key_ptr, entry.value_ptr });
                    i += 1;
                }
                try writer.print("}}", .{});
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
    blockStatement: *BlockStatement,

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
            .blockStatement => |bs| {
                try writer.print("{s}", .{bs});
            },
        }
    }
};

pub const BlockStatement = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    statements: ArrayList(*Statement),

    pub fn init(allocator: std.mem.Allocator) *BlockStatement {
        var block = allocator.create(BlockStatement) catch {
            std.debug.panic("Failed to allocate BlockStatement", .{});
        };

        block.* = .{ .statements = ArrayList(*Statement).init(allocator), .allocator = allocator };
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

        // try writer.writeAll("{");
        for (self.statements.items) |statement| {
            try writer.print("{s}", .{statement});
        }
        // try writer.writeAll("}");
    }
};

pub const Identifier = struct {
    identifier: []const u8,

    pub fn init(ident: []const u8) Identifier {
        return .{ .identifier = ident };
    }
};
