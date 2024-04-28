const std = @import("std");

pub const Expression = union(enum) {
    identifier: Identifier,
};

pub const LetStatement = struct {
    identifier: Identifier,
    expression: Expression,
};

pub const Statement = union(enum) {
    letStatement: LetStatement,
};

pub const Identifier = struct {
    identifier: []const u8,

    pub fn init(ident: []const u8) Identifier {
        return .{ .identifier = ident };
    }
};
