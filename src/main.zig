const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const evaluator = @import("evaluator.zig");
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const environment = @import("environment.zig");

// monkey face in zig multi-line string constant
const monkeyFace =
    \\.
    \\          __
    \\     w  c(oo)o   (
    \\      \__(-)    __)
    \\          /\   (
    \\         /(_)___)
    \\        w  /|
    \\        |  |
    \\        m  m
;

const prompt: []const u8 = ">> ";

fn start() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const in = std.io.getStdIn();
    var buf = std.io.bufferedReader(in.reader());
    var r = buf.reader();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    while (true) {
        try stdout.print("{s}", .{prompt});
        try bw.flush();

        var line_buf: [4096]u8 = undefined;
        var line = try r.readUntilDelimiterOrEof(&line_buf, '\n');

        if (line) |input| {
            var l = lexer.Lexer.init(input);

            var p = parser.Parser.init(l, gpa.allocator());
            defer p.deinit();

            var program = p.parseProgram();

            if (p.errors.items.len > 0) {
                printParserErrors(p.errors);
                continue;
            }

            var node = .{ .program = &program };

            var env = environment.Environment.init(gpa.allocator());
            defer env.deinit();
            var e = evaluator.Evaluator.init(gpa.allocator());
            defer e.deinit();

            var evaluated = e.eval(node, &env);

            if (evaluated) |result| {
                try stdout.print("{s}\n", .{result});
            }
        }
    }
    defer gpa.deinit();
}

fn printParserErrors(errors: ArrayListUnmanaged([]u8)) void {
    std.debug.print("{s}\n", .{monkeyFace});
    std.debug.print("Woops! We ran into some monkey business here!\n", .{});
    std.debug.print(" parser errors:\n", .{});
    for (errors.items) |err| {
        std.debug.print("\t{s}\n", .{err});
    }
}

pub fn main() !void {
    // const allocator = std.heap.page_allocator();
    if (std.os.getenv("USER")) |user| {
        // Successfully retrieved the variable
        std.debug.print("Hello {s}! This is the Monkey Programming Language!\n", .{user});
        std.debug.print("Feel free to type in commands\n", .{});
        try start();
    }
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)main
    // std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.

    // try stdout.print("Run `zig build test` to run the tests.\n", .{});

    // try bw.flush(); // don't forget to flush!
}

test "simple test" {
    // std.debug.print("here...", .{});
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test {
    _ = @import("lexer.zig");
}
