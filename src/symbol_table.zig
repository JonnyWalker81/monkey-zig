const std = @import("std");

pub const SymbolScope = enum {
    global,
    local,
    builtin,

    pub const SymbolNameTable = [@typeInfo(SymbolScope).Enum.fields.len][:0]const u8{
        "global",
        "local",
        "builtin",
    };

    pub fn str(self: SymbolScope) [:0]const u8 {
        return SymbolNameTable[@intFromEnum(self)];
    }
};

pub const Symbol = struct {
    name: []const u8,
    scope: SymbolScope,
    index: usize,
};

pub const SymbolTable = struct {
    const Self = @This();
    // arena: std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,
    store: std.StringHashMap(Symbol),
    num_definitions: i32,
    outer: ?*SymbolTable,

    pub fn init(allocator: std.mem.Allocator) *SymbolTable {
        // var arena = std.heap.ArenaAllocator.init(allocator);
        const st = allocator.create(SymbolTable) catch unreachable;
        st.* = SymbolTable{
            // .arena = arena,
            .allocator = allocator,
            .store = std.StringHashMap(Symbol).init(allocator),
            .num_definitions = 0,
            .outer = null,
        };

        return st;
    }

    pub fn initEnclosedScope(allocator: std.mem.Allocator, outer: *SymbolTable) *SymbolTable {
        // var arena = std.heap.ArenaAllocator.init(allocator);
        const st = allocator.create(SymbolTable) catch unreachable;
        st.* = SymbolTable{
            // .arena = arena,
            .allocator = allocator,
            .store = std.StringHashMap(Symbol).init(allocator),
            .num_definitions = 0,
            .outer = outer,
        };

        return st;
    }

    pub fn deinit(self: *Self) void {
        // if (self.outer) |outer| {
        //     outer.deinit();
        // }

        self.store.deinit();
        // self.arena.deinit();
        // self.allocator.destroy(self);
    }

    pub fn define(self: *Self, name: []const u8) !Symbol {
        // std.log.warn("define: {s}", .{name});
        var symbol: Symbol = undefined;

        // std.log.warn("define: {s}", .{name});
        // std.log.warn("define outer: {any}", .{self.outer});
        if (self.outer == null) {
            symbol = .{
                .name = name,
                .scope = SymbolScope.global,
                .index = @intCast(self.num_definitions),
            };
        } else {
            symbol = .{
                .name = name,
                .scope = SymbolScope.local,
                .index = @intCast(self.num_definitions),
            };
        }

        try self.store.put(name, symbol);
        self.num_definitions += 1;
        return symbol;
    }

    pub fn defineBuiltin(self: *Self, index: usize, name: []const u8) !Symbol {
        const symbol = .{
            .name = name,
            .scope = SymbolScope.builtin,
            .index = index,
        };

        try self.store.put(name, symbol);
        return symbol;
    }

    pub fn resolve(self: *Self, name: []const u8) ?Symbol {
        const obj = self.store.get(name);
        if (obj == null and self.outer != null) {
            if (self.outer) |outer| {
                return outer.resolve(name);
            }
        }

        return obj;
    }
};

const test_allocator = std.testing.allocator;
const expectEqual = std.testing.expectEqual;
test "test define" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();

    const expected = SymbolTable.init(arena.allocator());
    // defer test_allocator.destroy(expected);

    try expected.store.put("a", Symbol{ .name = "a", .scope = SymbolScope.global, .index = 0 });
    try expected.store.put("b", Symbol{ .name = "b", .scope = SymbolScope.global, .index = 1 });
    try expected.store.put("c", Symbol{ .name = "c", .scope = SymbolScope.local, .index = 0 });
    try expected.store.put("d", Symbol{ .name = "d", .scope = SymbolScope.local, .index = 1 });
    try expected.store.put("e", Symbol{ .name = "e", .scope = SymbolScope.local, .index = 0 });
    try expected.store.put("f", Symbol{ .name = "f", .scope = SymbolScope.local, .index = 1 });

    var global = SymbolTable.init(arena.allocator());
    // defer global.deinit();
    // defer test_allocator.destroy(global);

    const a = try global.define("a");
    try expectEqual(a, expected.store.get("a").?);

    const b = try global.define("b");
    try expectEqual(b, expected.store.get("b").?);

    var firstLocal = SymbolTable.initEnclosedScope(arena.allocator(), global);
    defer firstLocal.deinit();
    // defer test_allocator.destroy(firstLocal);

    const c = try firstLocal.define("c");
    try expectEqual(c, expected.store.get("c").?);
    const d = try firstLocal.define("d");
    try expectEqual(d, expected.store.get("d").?);

    var secondLocal = SymbolTable.initEnclosedScope(arena.allocator(), firstLocal);
    // defer secondLocal.deinit();
    // defer test_allocator.destroy(secondLocal);

    const e = try secondLocal.define("e");
    try expectEqual(e, expected.store.get("e").?);
    const f = try secondLocal.define("f");
    try expectEqual(f, expected.store.get("f").?);
}

test "test resolve global" {
    var global = SymbolTable.init(test_allocator);
    defer test_allocator.destroy(global);
    defer global.deinit();

    _ = try global.define("a");
    _ = try global.define("b");

    const expected = [_]Symbol{
        Symbol{ .name = "a", .scope = SymbolScope.global, .index = 0 },
        Symbol{ .name = "b", .scope = SymbolScope.global, .index = 1 },
    };

    for (expected) |sym| {
        const result = global.resolve(sym.name);
        try expectEqual(result, sym);
    }
}

test "test resolve local" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();
    var global = SymbolTable.init(arena.allocator());
    // defer global.deinit();
    // defer test_allocator.destroy(global);
    _ = try global.define("a");
    _ = try global.define("b");

    var local = SymbolTable.initEnclosedScope(arena.allocator(), global);
    // defer test_allocator.destroy(local);
    // defer local.deinit();
    _ = try local.define("c");
    _ = try local.define("d");

    const expected = [_]Symbol{
        Symbol{ .name = "a", .scope = SymbolScope.global, .index = 0 },
        Symbol{ .name = "b", .scope = SymbolScope.global, .index = 1 },
        Symbol{ .name = "c", .scope = SymbolScope.local, .index = 0 },
        Symbol{ .name = "d", .scope = SymbolScope.local, .index = 1 },
    };

    for (expected) |sym| {
        const result = local.resolve(sym.name);
        try expectEqual(result, sym);
    }
}

test "test resolves nested local" {
    var global = SymbolTable.init(test_allocator);
    defer test_allocator.destroy(global);
    defer global.deinit();
    _ = try global.define("a");
    _ = try global.define("b");

    var firstLocal = SymbolTable.initEnclosedScope(test_allocator, global);
    defer test_allocator.destroy(firstLocal);
    defer firstLocal.deinit();
    _ = try firstLocal.define("c");
    _ = try firstLocal.define("d");

    var secondLocal = SymbolTable.initEnclosedScope(test_allocator, firstLocal);
    defer test_allocator.destroy(secondLocal);
    defer secondLocal.deinit();
    _ = try secondLocal.define("e");
    _ = try secondLocal.define("f");

    const tests = [_]struct {
        table: *SymbolTable,
        expectedSymbols: []const Symbol,
    }{
        .{
            .table = firstLocal,
            .expectedSymbols = &[_]Symbol{
                Symbol{ .name = "a", .scope = SymbolScope.global, .index = 0 },
                Symbol{ .name = "b", .scope = SymbolScope.global, .index = 1 },
                Symbol{ .name = "c", .scope = SymbolScope.local, .index = 0 },
                Symbol{ .name = "d", .scope = SymbolScope.local, .index = 1 },
            },
        },
        .{
            .table = secondLocal,
            .expectedSymbols = &[_]Symbol{
                Symbol{ .name = "a", .scope = SymbolScope.global, .index = 0 },
                Symbol{ .name = "b", .scope = SymbolScope.global, .index = 1 },
                Symbol{ .name = "e", .scope = SymbolScope.local, .index = 0 },
                Symbol{ .name = "f", .scope = SymbolScope.local, .index = 1 },
            },
        },
    };

    for (tests) |tt| {
        for (tt.expectedSymbols) |sym| {
            const result = tt.table.resolve(sym.name);
            try expectEqual(result, sym);
        }
    }
}

test "test define resolve builtins" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();
    var global = SymbolTable.init(arena.allocator());
    // defer global.deinit();
    // defer test_allocator.destroy(global);
    const firstLocal = SymbolTable.initEnclosedScope(arena.allocator(), global);
    const secondLocal = SymbolTable.initEnclosedScope(arena.allocator(), firstLocal);

    const expected = &[_]Symbol{
        .{ .name = "a", .scope = SymbolScope.builtin, .index = 0 },
        .{ .name = "c", .scope = SymbolScope.builtin, .index = 1 },
        .{ .name = "e", .scope = SymbolScope.builtin, .index = 2 },
        .{ .name = "f", .scope = SymbolScope.builtin, .index = 3 },
    };

    for (expected, 0..) |sym, i| {
        _ = try global.defineBuiltin(i, sym.name);
    }

    const tables = &[_]*SymbolTable{
        global,
        firstLocal,
        secondLocal,
    };

    for (tables) |table| {
        for (expected) |sym| {
            const result = table.resolve(sym.name);
            try expectEqual(result.?, sym);
        }
    }
}
