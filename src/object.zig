const std = @import("std");
const ast = @import("ast.zig");
const environment = @import("environment.zig");
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;
const Wyhash = std.hash.Wyhash;
const code = @import("code.zig");

pub const BuiltinFn = *const fn (allocator: std.mem.Allocator, []*Object) ?*Object;

pub const HashKey = struct {
    const Self = @This();
    type: []const u8,
    value: u64,
};

pub const HashKeyContext = struct {
    const Self = @This();
    pub fn hash(_: Self, key: HashKey) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHashStrat(&hasher, key.type, .Deep);
        std.hash.autoHashStrat(&hasher, key.value, .Deep);
        return hasher.final();
    }

    pub fn eql(_: Self, a: HashKey, b: HashKey) bool {
        return std.mem.eql(u8, a.type, b.type) and a.value == b.value;
    }
};

pub const HashPair = struct {
    key: *Object,
    value: *Object,
};

pub const CompiledFunction = struct {
    const Self = @This();

    instructions: code.Instructions,
    numLocals: i32,
    numParameters: i32,
};

pub const Function = struct {
    const Self = @This();

    parameters: ArrayList(*ast.Identifier),
    body: *ast.BlockStatement,
    env: *environment.Environment,
};

pub const Closure = struct {
    const Self = @This();

    func: CompiledFunction,
    free: ArrayList(Object),
};

pub const Object = union(enum) {
    const Self = @This();
    // allocator: std.mem.Allocator,

    nil,
    integer: i64,
    boolean: bool,
    returnValue: *Self,
    function: Function,
    compiledFunction: CompiledFunction,
    closure: Closure,
    string: []const u8,
    builtin: BuiltinFn,
    array: ArrayList(*Self),
    hash: struct {
        pairs: std.HashMap(HashKey, HashPair, HashKeyContext, std.hash_map.default_max_load_percentage),
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

    pub fn intValue(self: *const Self) i64 {
        return switch (self.*) {
            .integer => |i| i,
            else => -1,
        };
    }

    pub fn boolValue(self: *const Self) bool {
        return switch (self.*) {
            .boolean => |b| b,
            .integer => |_| true,
            else => false,
        };
    }

    pub fn stringValue(self: *const Self) []const u8 {
        return switch (self.*) {
            .string => |s| s,
            else => "nil",
        };
    }

    pub fn typeId(self: *const Self) []const u8 {
        return switch (self.*) {
            .nil => "NULL",
            .integer => "INTEGER",
            .boolean => "BOOLEAN",
            .function => "FUNCTION",
            .err => "ERROR",
            .returnValue => |r| r.typeId(),
            .string => "STRING",
            .builtin => "BUILTIN",
            .array => "ARRAY",
            .hash => "HASH",
            .compiledFunction => "COMPILED_FUNCTION",
            .closure => "CLOSURE",
        };
    }

    pub fn hashKey(self: *const Self) HashKey {
        return switch (self.*) {
            .integer => |i| .{ .type = "INTEGER", .value = @as(u64, @intCast(i)) },
            .boolean => |b| .{ .type = "BOOLEAN", .value = if (b) 1 else 0 },
            .string => |s| {
                return .{ .type = "STRING", .value = Wyhash.hash(0, s) };
            },
            else => .{ .type = "NULL", .value = 0 },
        };
    }

    pub fn compiledFn(self: *const Self) CompiledFunction {
        return switch (self.*) {
            .compiledFunction => |cf| cf,
            else => std.debug.panic("not a compiled function", .{}),
        };
    }

    pub fn builtinFn(self: *const Self) BuiltinFn {
        return switch (self.*) {
            .builtin => |b| b,
            else => std.debug.panic("not a builtin function", .{}),
        };
    }

    pub fn closureFn(self: *const Self) Closure {
        return switch (self.*) {
            .closure => |c| c,
            else => std.debug.panic("not a closure", .{}),
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
                try writer.print("){{ ", .{});
                try writer.print("{s}", .{f.body});
                try writer.print("}}", .{});
            },
            .err => |e| {
                try writer.print("{s}", .{e});
            },
            .string => |s| {
                try writer.print("{s}", .{s});
            },
            .builtin => {
                try writer.print("builtin function", .{});
            },
            .array => |a| {
                try writer.print("[", .{});
                for (a.items, 0..) |item, i| {
                    try writer.print("{s}", .{item});
                    if (i != a.items.len - 1) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print("]", .{});
            },
            .hash => |h| {
                var it = h.pairs.iterator();
                try writer.print("{{", .{});
                var i: usize = 0;
                while (it.next()) |pair| {
                    if (i > 0) {
                        try writer.print(", ", .{});
                    }
                    try writer.print("{any} : {any}", .{ pair.key_ptr, pair.value_ptr });
                    i += 1;
                }
                try writer.print("}}", .{});
            },
            .compiledFunction => |cf| {
                try writer.print("CompiledFunction: {any}", .{cf});
            },
            .closure => |c| {
                try writer.print("Closure[{any}]", .{c});
            },
        }
    }
};

const assert = std.debug.assert;
const expectEqualDeep = std.testing.expectEqualDeep;
const test_allocator = std.testing.allocator;
test "test string hash key" {
    var hello1: *Object = try test_allocator.create(Object);
    defer test_allocator.destroy(hello1);
    hello1.* = .{ .string = "Hello World" };

    var hello2: *Object = try test_allocator.create(Object);
    defer test_allocator.destroy(hello2);
    hello2.* = .{ .string = "Hello World" };

    var diff1: *Object = try test_allocator.create(Object);
    defer test_allocator.destroy(diff1);
    diff1.* = .{ .string = "My name is johnny" };

    var diff2: *Object = try test_allocator.create(Object);
    defer test_allocator.destroy(diff2);
    diff2.* = .{ .string = "My name is johnny" };

    try expectEqualDeep(hello1.hashKey(), hello2.hashKey());
    try expectEqualDeep(diff1.hashKey(), diff2.hashKey());
    assert(hello1.hashKey().value != diff1.hashKey().value);
    assert(hello2.hashKey().value != diff2.hashKey().value);
}
