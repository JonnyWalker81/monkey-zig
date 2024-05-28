const std = @import("std");
const object = @import("object.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const compiler = @import("compiler.zig");
const vm = @import("vm.zig");
const evaluator = @import("evaluator.zig");
const env = @import("environment.zig");
const code = @import("code.zig");

const input =
    \\
    \\let fibonacci = fn(x) {
    \\if (x == 0) { 0
    \\     } else {
    \\       if (x == 1) {
    \\         return 1;
    \\       } else {
    \\         fibonacci(x - 1) + fibonacci(x - 2);
    \\       }
    \\} };
    \\   fibonacci(50);
;

pub fn main() !void {
    var args = std.process.args();
    _ = args.skip(); //to skip the zig call

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    const engine = args.next();

    var duration: i64 = undefined;
    var result: *object.Object = undefined;

    var l = lexer.Lexer.init(arena.allocator(), input);
    defer l.deinit();

    var p = parser.Parser.init(l, arena.allocator());
    defer p.deinit();

    const prog = p.parseProgram();

    var definitions = try code.initDefinitions(arena.allocator());
    defer definitions.deinit(arena.allocator());

    if (engine) |eng| {
        std.debug.print("running with {s}\n", .{eng});
        if (std.mem.containsAtLeast(u8, eng, 1, "vm")) {
            var comp = compiler.Compiler.init(arena.allocator(), definitions);
            defer comp.deinit();

            try comp.compile(.{ .program = prog });

            var machine = vm.VM.init(arena.allocator(), comp.bytecode());
            defer machine.deinit();

            const start = std.time.milliTimestamp();

            machine.run() catch |e| {
                std.debug.print("VM Error: {any}\n", .{e});
                return;
            };

            const end = std.time.milliTimestamp();
            duration = end - start;
            const r = machine.lastPoppedStackElem();
            result = try arena.allocator().create(object.Object);
            result.* = r;
        } else {
            std.debug.print("Using Evaluator\n", .{});
            var eval = evaluator.Evaluator.init(arena.allocator());
            defer eval.deinit();

            var e = env.Environment.init(arena.allocator());
            defer e.deinit();

            const start = std.time.milliTimestamp();
            result = eval.eval(.{ .program = prog }, e).?;
            const end = std.time.milliTimestamp();
            duration = end - start;
        }
    }

    std.debug.print("engine={any} result={s} duration={any}\n", .{ engine, result, duration });
}
