const std = @import("std");
const object = @import("object.zig");
const StringHashMap = std.StringHashMap;

pub const Environment = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    store: StringHashMap(*object.Object),

    pub fn init(allocator: std.mem.Allocator) Environment {
        return Environment{ .allocator = allocator, .store = StringHashMap(*object.Object).init(allocator) };
    }

    pub fn deinit(self: *Self) void {
        self.store.deinit();
    }

    pub fn get(self: *Self, key: []const u8) ?*object.Object {
        return self.store.get(key);
    }

    pub fn set(self: *Self, key: []const u8, value: *object.Object) *object.Object {
        self.store.put(key, value) catch |err| {
            std.debug.print("Failed to set environment variable: {}\n", .{err});
        };
        return value;
    }
};
