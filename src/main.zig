const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const evaluator = @import("evaluator.zig");
const ArrayList = std.ArrayList;
const environment = @import("environment.zig");
const code = @import("code.zig");
const compiler = @import("compiler.zig");
const vm = @import("vm.zig");
const object = @import("object.zig");
const sym = @import("symbol_table.zig");

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

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    var definitions = try code.initDefinitions(arena.allocator());
    defer definitions.deinit(arena.allocator());

    var env = environment.Environment.init(arena.allocator());
    defer env.deinit();

    var constants = std.ArrayList(object.Object).init(arena.allocator());

    var globals: [vm.GlobalSize]object.Object = undefined;
    @memset(&globals, .nil);

    const symbolTable = sym.SymbolTable.init(arena.allocator());

    while (true) {
        // defer _ = gpa.deinit();

        try stdout.print("{s}", .{prompt});
        try bw.flush();

        var line_buf: [4096]u8 = undefined;
        const line = try r.readUntilDelimiterOrEof(&line_buf, '\n');

        if (line) |input| {
            var l = lexer.Lexer.init(arena.allocator(), input);
            defer l.deinit();

            var p = parser.Parser.init(l, arena.allocator());
            defer p.deinit();

            const program = p.parseProgram();

            if (p.errors.items.len > 0) {
                printParserErrors(p.errors);
                continue;
            }

            const node = .{ .program = program };

            var comp = compiler.Compiler.initWithState(arena.allocator(), definitions, symbolTable, &constants);
            try comp.compile(node);

            var machine = vm.VM.initWithGlobalStore(arena.allocator(), comp.bytecode(), &globals);
            try machine.run();

            const lastPopped = machine.lastPoppedStackElem();
            try stdout.print("{s}\n", .{lastPopped});

            // var e = evaluator.Evaluator.init(arena.allocator());
            // defer e.deinit();

            // const evaluated = e.eval(node, env);

            // if (evaluated) |result| {
            //     try stdout.print("{s}\n", .{result});
            // }
        }
    }
}

fn printParserErrors(errors: ArrayList([]u8)) void {
    std.debug.print("{s}\n", .{monkeyFace});
    std.debug.print("Woops! We ran into some monkey business here!\n", .{});
    std.debug.print(" parser errors:\n", .{});
    for (errors.items) |err| {
        std.debug.print("\t{s}\n", .{err});
    }
}

pub fn main() !void {
    // const allocator = std.heap.page_allocator();
    if (std.posix.getenv("USER")) |user| {
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
