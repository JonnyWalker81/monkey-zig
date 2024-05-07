const std = @import("std");
const object = @import("object.zig");
const StringHashMap = std.StringHashMap;

pub const Environment = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    store: StringHashMap(*object.Object),
    outer: ?*Self = null,

    pub fn init(allocator: std.mem.Allocator) *Environment {
        const env = allocator.create(Environment) catch unreachable;
        env.* = Environment{ .allocator = allocator, .store = StringHashMap(*object.Object).init(allocator) };
        return env;
        // return Environment{ .allocator = allocator, .store = StringHashMap(*object.Object).init(allocator) };
    }

    pub fn initWithEnclosedEnv(allocator: std.mem.Allocator, outer: *Self) *Environment {
        const newEnv = allocator.create(Environment) catch unreachable;
        newEnv.* = Environment{ .allocator = allocator, .store = StringHashMap(*object.Object).init(allocator), .outer = outer };
        return newEnv;
    }

    pub fn deinit(self: *Self) void {
        self.store.deinit();
        if (self.outer) |outer| {
            outer.deinit();
        }

        self.allocator.destroy(self);
    }

    pub fn get(self: *Self, key: []const u8) ?*object.Object {
        std.log.warn("Getting key: {s} {d}\n", .{ key, self.store.count() });
        var iterator = self.store.keyIterator();

        while (iterator.next()) |entry| {
            std.log.warn("Entry: {s} \n", .{entry.*});
        }
        if (self.store.get(key)) |value| {
            std.log.warn("Getting key (inner): {s} {d}\n", .{ key, self.store.count() });
            return value;
        }

        if (self.outer) |outer| {
            std.log.warn("Getting key (outer): {s} {d}\n", .{ key, outer.store.count() });
            return outer.get(key);
        }

        return null;
    }

    pub fn set(self: *Self, key: []const u8, value: *object.Object) *object.Object {
        std.log.warn("(before) Setting key: {s} to {any}, count: {d}\n", .{ key, value, self.store.count() });
        self.store.put(key, value) catch |err| {
            std.debug.print("Failed to set environment variable: {}\n", .{err});
        };
        // std.log.warn("(after) Setting key: {s} to {any}, count: {d}\n", .{ key, value, self.store.count() });
        return value;
    }
};
