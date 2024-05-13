const std = @import("std");

pub fn flatten(allocator: std.mem.Allocator, arr: []const []const u8) ![]u8 {
    var total_len: usize = 0;
    for (arr) |sub_arr| {
        total_len += sub_arr.len;
    }

    var result = try allocator.alloc(u8, total_len);

    var offset: usize = 0;
    for (arr) |sub_arr| {
        for (sub_arr, 0..) |elem, i| {
            result[offset + i] = elem;
        }
        offset += sub_arr.len;
    }

    return result;
}
