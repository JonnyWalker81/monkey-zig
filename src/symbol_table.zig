const std = @import("std");

pub const SymbolScope = enum {
    Global,
    // Local,
    pub const SymbolNameTable = [@typeInfo(SymbolScope).Enum.fields.len][:0]const u8{
        "Global",
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
    allocator: std.mem.Allocator,
    store: std.StringHashMap(Symbol),
    num_definitions: usize,

    pub fn init(allocator: std.mem.Allocator) *SymbolTable {
        const st = allocator.create(SymbolTable) catch unreachable;
        st.* = SymbolTable{
            .allocator = allocator,
            .store = std.StringHashMap(Symbol).init(allocator),
            .num_definitions = 0,
        };

        return st;
    }

    pub fn deinit(self: *SymbolTable) void {
        self.store.deinit();
    }

    pub fn define(self: *SymbolTable, name: []const u8) !Symbol {
        const symbol = Symbol{
            .name = name,
            .scope = SymbolScope.Global,
            .index = self.num_definitions,
        };
        try self.store.put(name, symbol);
        self.num_definitions += 1;
        return symbol;
    }

    pub fn resolve(self: *SymbolTable, name: []const u8) ?Symbol {
        return self.store.get(name);
    }
};

const test_allocator = std.testing.allocator;
const expectEqual = std.testing.expectEqual;
test "test define" {
    var expected = std.StringHashMap(Symbol).init(test_allocator);
    defer expected.deinit();
    try expected.put("a", Symbol{ .name = "a", .scope = SymbolScope.Global, .index = 0 });
    try expected.put("b", Symbol{ .name = "b", .scope = SymbolScope.Global, .index = 1 });

    var global = SymbolTable.init(test_allocator);
    defer test_allocator.destroy(global);
    defer global.deinit();

    const a = try global.define("a");
    try expectEqual(a, expected.get("a").?);

    const b = try global.define("b");
    try expectEqual(b, expected.get("b").?);
}

test "test resolve global" {
    var global = SymbolTable.init(test_allocator);
    defer test_allocator.destroy(global);
    defer global.deinit();
    _ = try global.define("a");
    _ = try global.define("b");

    const expected = [_]Symbol{
        Symbol{ .name = "a", .scope = SymbolScope.Global, .index = 0 },
        Symbol{ .name = "b", .scope = SymbolScope.Global, .index = 1 },
    };

    for (expected) |sym| {
        const result = global.resolve(sym.name);
        try expectEqual(result, sym);
    }
}
