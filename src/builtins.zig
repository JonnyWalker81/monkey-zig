const std = @import("std");
const object = @import("object.zig");

const Builtins = [_]struct {
    name: []const u8,
    Builtin: object.Object,
}{
    .{ .name = "len", .Builtin = .{ .builtin = len } },
    .{ .name = "puts", .Builtin = .{ .builtin = puts } },
    .{ .name = "first", .Builtin = .{ .builtin = first } },
    .{ .name = "last", .Builtin = .{ .builtin = last } },
    .{ .name = "rest", .Builtin = .{ .builtin = rest } },
    .{ .name = "push", .Builtin = .{ .builtin = push } },
};

fn len(allocator: std.mem.Allocator, args: []*object.Object) ?*object.Object {
    if (args.len != 1) {
        return new_error(allocator, "wrong number of arguments. got={d}, want=1", .{args.len});
    }

    // var a: *object.Object = a[0];
    switch (args[0].*) {
        .string => |s| {
            const obj: *object.Object = allocator.create(object.Object) catch std.debug.panic("failed to allocate object", .{});
            obj.* = .{ .integer = @as(i64, @intCast(s.len)) };
            return obj;
        },
        .array => |arr| {
            const obj: *object.Object = allocator.create(object.Object) catch std.debug.panic("failed to allocate object", .{});
            obj.* = .{ .integer = @as(i64, @intCast(arr.items.len)) };
            return obj;
        },
        else => {
            return new_error(allocator, "argument to `len` not supported, got {s}", .{args[0].typeId()});
        },
    }
}

fn first(allocator: std.mem.Allocator, args: []*object.Object) ?*object.Object {
    if (args.len != 1) {
        return new_error(allocator, "wrong number of arguments. got={d}, want=1", .{args.len});
    }

    if (args[0].* != .array) {
        return new_error(allocator, "argument to `first` must be ARRAY, got {s}", .{args[0].typeId()});
    }

    var obj: *object.Object = allocator.create(object.Object) catch std.debug.panic("failed to allocate object", .{});
    const arr = args[0].array;
    if (arr.items.len > 0) {
        obj = arr.items[0];
        return obj;
    }

    obj.* = .nil;
    return obj;
}

pub fn last(allocator: std.mem.Allocator, args: []*object.Object) ?*object.Object {
    if (args.len != 1) {
        return new_error(allocator, "wrong number of arguments. got={d}, want=1", .{args.len});
    }

    if (args[0].* != .array) {
        return new_error(allocator, "argument to `last` must be ARRAY, got {s}", .{args[0].typeId()});
    }

    var obj: *object.Object = allocator.create(object.Object) catch std.debug.panic("failed to allocate object", .{});
    const arr = args[0].array;
    if (arr.items.len > 0) {
        obj = arr.items[arr.items.len - 1];
        return obj;
    }

    obj.* = .nil;
    return obj;
}

pub fn rest(allocator: std.mem.Allocator, args: []*object.Object) ?*object.Object {
    if (args.len != 1) {
        return new_error(allocator, "wrong number of arguments. got={d}, want=1", .{args.len});
    }

    if (args[0].* != .array) {
        return new_error(allocator, "argument to `rest` must be ARRAY, got {s}", .{args[0].typeId()});
    }

    const obj: *object.Object = allocator.create(object.Object) catch std.debug.panic("failed to allocate object", .{});
    const arr = args[0].array;
    if (arr.items.len > 0) {
        var newArr = std.ArrayList(*object.Object).init(allocator);
        for (1..arr.items.len) |i| {
            newArr.append(arr.items[i]) catch unreachable;
        }
        obj.* = .{ .array = newArr };
        return obj;
    }

    obj.* = .nil;
    return obj;
}

pub fn push(allocator: std.mem.Allocator, args: []*object.Object) ?*object.Object {
    if (args.len != 2) {
        return new_error(allocator, "wrong number of arguments. got={d}, want=2", .{args.len});
    }

    if (args[0].* != .array) {
        return new_error(allocator, "argument to `push` must be ARRAY, got {s}", .{args[0].typeId()});
    }

    const obj: *object.Object = allocator.create(object.Object) catch std.debug.panic("failed to allocate object", .{});
    const arr = args[0].array;
    var newArr = std.ArrayList(*object.Object).init(allocator);
    for (arr.items) |item| {
        newArr.append(item) catch unreachable;
    }
    newArr.append(args[1]) catch unreachable;
    obj.* = .{ .array = newArr };
    return obj;
}

pub fn puts(allocator: std.mem.Allocator, args: []*object.Object) ?*object.Object {
    for (args) |arg| {
        // _ = std.io.getStdOut().write(arg.stringValue()) catch unreachable;
        // _ = std.io.getStdOut().write("\n") catch unreachable;
        std.log.info("{s}\n", .{arg.stringValue()});
    }

    const obj: *object.Object = allocator.create(object.Object) catch std.debug.panic("failed to allocate object", .{});
    obj.* = .nil;
    return obj;
}

pub fn get_builtin_by_name(name: []const u8) ?object.Object {
    for (Builtins) |b| {
        if (std.mem.eql(u8, b.name, name)) {
            return b.Builtin;
        }
    }
    return null;
}

pub fn new_error(allocator: std.mem.Allocator, comptime fmt: []const u8, args: anytype) *object.Object {
    const obj: *object.Object = allocator.create(object.Object) catch std.debug.panic("failed to allocate object", .{});
    const msg = std.fmt.allocPrint(allocator, fmt, args) catch unreachable;
    obj.* = .{ .err = msg };
    return obj;
}
