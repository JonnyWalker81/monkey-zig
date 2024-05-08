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
        var iter = self.store.iterator();
        while (iter.next()) |kv| {
            self.allocator.free(kv.key_ptr.*);
            // kv.value_ptr.deinit();
        }

        self.store.deinit();
        if (self.outer) |outer| {
            var iterOuter = outer.store.iterator();
            while (iterOuter.next()) |kv| {
                self.allocator.free(kv.key_ptr.*);
                // kv.value_ptr.deinit();
            }
            outer.deinit();
        }

        self.allocator.destroy(self);
    }

    pub fn get(self: *Self, key: []const u8) ?*object.Object {
        // std.log.warn("Getting key: {s} {d}\n", .{ key, self.store.count() });
        // var iterator = self.store.keyIterator();

        // while (iterator.next()) |entry| {
        //     std.log.warn("Entry: {s} \n", .{entry.*});
        // }
        if (self.store.get(key)) |value| {
            // std.log.warn("Getting key (inner): {s} {d}\n", .{ key, self.store.count() });
            return value;
        }

        if (self.outer) |outer| {
            // std.log.warn("Getting key (outer): {s} {d}\n", .{ key, outer.store.count() });
            return outer.get(key);
        }

        // std.log.warn("Getting key (not found): {s} {d}\n", .{ key, self.store.count() });
        return null;
    }

    pub fn set(self: *Self, key: []const u8, value: *object.Object) *object.Object {
        // std.log.warn("Setting key: {s} to {any}, count: {d}\n", .{ key, value, self.store.count() });
        const owned_key: []u8 = self.allocator.dupe(u8, key) catch unreachable;
        // std.log.warn("(before) Setting key: {s} to {any}, count: {d}\n", .{ owned_key, value, self.store.count() });
        self.store.put(owned_key, value) catch |err| {
            std.debug.print("Failed to set environment variable: {}\n", .{err});
        };
        // std.log.warn("(after) Setting key: {s} to {any}, count: {d}\n", .{ key, value, self.store.count() });
        return value;
    }
};
