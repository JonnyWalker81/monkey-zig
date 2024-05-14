const std = @import("std");
const object = @import("object.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const program = @import("program.zig");
const ast = @import("ast.zig");
const environment = @import("environment.zig");
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const ArrayHashMap = std.ArrayHashMap;
const AutoHashMap = std.AutoHashMap;

// pub const BuiltinFn = *const fn (allocator: std.mem.Allocator, ArrayList(*object.Object)) ?object.Object;

pub fn load_builtins(allocator: std.mem.Allocator) !StringHashMap(*object.Object) {
    const alloc = allocator;
    var builtinFns = StringHashMap(*object.Object).init(alloc);

    const lenObj = try allocator.create(object.Object);
    lenObj.* = .{ .builtin = len };
    try builtinFns.put("len", lenObj);

    const firstObj = try allocator.create(object.Object);
    firstObj.* = .{ .builtin = first };
    try builtinFns.put("first", firstObj);

    const lastObj = try allocator.create(object.Object);
    lastObj.* = .{ .builtin = last };
    try builtinFns.put("last", lastObj);

    const restObj = try allocator.create(object.Object);
    restObj.* = .{ .builtin = rest };
    try builtinFns.put("rest", restObj);

    const pushObj = try allocator.create(object.Object);
    pushObj.* = .{ .builtin = push };
    try builtinFns.put("push", pushObj);

    const putsObj = try allocator.create(object.Object);
    putsObj.* = .{ .builtin = puts };
    try builtinFns.put("puts", putsObj);

    return builtinFns;
}

pub fn len(allocator: std.mem.Allocator, args: []*object.Object) ?*object.Object {
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

pub fn first(allocator: std.mem.Allocator, args: []*object.Object) ?*object.Object {
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
        var newArr = ArrayList(*object.Object).init(allocator);
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
    var newArr = ArrayList(*object.Object).init(allocator);
    for (arr.items) |item| {
        newArr.append(item) catch unreachable;
    }
    newArr.append(args[1]) catch unreachable;
    obj.* = .{ .array = newArr };
    return obj;
}

pub fn puts(allocator: std.mem.Allocator, args: []*object.Object) ?*object.Object {
    for (args) |arg| {
        _ = std.io.getStdOut().write(arg.stringValue()) catch unreachable;
        _ = std.io.getStdOut().write("\n") catch unreachable;
    }

    const obj: *object.Object = allocator.create(object.Object) catch std.debug.panic("failed to allocate object", .{});
    obj.* = .nil;
    return obj;
}

const NULL = .nil;
var TRUE = object.Object{ .boolean = true };
var FALSE = object.Object{ .boolean = false };

pub fn new_error(allocator: std.mem.Allocator, comptime fmt: []const u8, args: anytype) *object.Object {
    const obj: *object.Object = allocator.create(object.Object) catch std.debug.panic("failed to allocate object", .{});
    const msg = std.fmt.allocPrint(allocator, fmt, args) catch unreachable;
    obj.* = .{ .err = msg };
    return obj;
}

pub const Evaluator = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    builtins: StringHashMap(*object.Object),

    pub fn init(allocator: std.mem.Allocator) Self {
        var arena = std.heap.ArenaAllocator.init(allocator);
        // const arena = std.heap.ArenaAllocator.init(allocator);
        const bFuncs = load_builtins(arena.allocator()) catch std.debug.panic("failed to load builtins", .{});

        return .{ .arena = arena, .builtins = bFuncs };
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }

    pub fn eval(self: *Self, node: ast.Node, env: *environment.Environment) ?*object.Object {
        switch (node) {
            .program => |p| {
                // std.log.warn("evaluating program...", .{});
                return self.eval_program(p, env);
            },
            .expression => |es| {
                return self.eval_expression(es, env);
            },
            .statement => |s| {
                return self.eval_statement(s, env);
            },
        }
    }

    fn eval_program(self: *Self, prog: program.Program, env: *environment.Environment) ?*object.Object {
        var result: ?*object.Object = undefined;
        for (prog.statements.items) |stmt| {
            // std.log.warn("(before) env: {any} {d}", .{ env, &env });
            result = self.eval_statement(stmt, env);
            // std.log.warn("(after) env: {any} {d}", .{ env, &env });

            if (result) |r| {
                // _ = env.set("foo", r);
                // std.log.warn("(after foo) env: {any} {d}", .{ env, &env });
                switch (r.*) {
                    .returnValue => |rv| {
                        return rv;
                    },
                    .err => |_| {
                        return result;
                    },
                    else => {},
                }
            }
        }
        return result;
    }

    // pub fn eval_program(self: *Self, statement: *ast.Statement) *object.Object {
    //     return self.eval_statement(statement);
    // }

    pub fn eval_statement(self: *Self, statement: *ast.Statement, env: *environment.Environment) ?*object.Object {
        switch (statement.*) {
            .letStatement => |ls| {
                const value = self.eval_expression(ls.expression, env);
                if (is_error(value)) {
                    return value;
                }
                if (value) |v| {
                    // std.log.warn("setting {s} to {s} -- {d}", .{ ls.identifier.identifier, v, &env });
                    _ = env.set(ls.identifier.identifier, v);
                }
                return value;
            },
            .expressionStatement => |es| {
                return self.eval_expression(es.expression, env);
            },
            .blockStatement => |bs| {
                var result = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                for (bs.statements.items) |stmt| {
                    result = self.eval_statement(stmt, env) orelse return null;
                }
                return result;
            },
            .returnStatement => |rs| {
                const result = self.eval_expression(rs.expression, env);
                if (is_error(result)) {
                    return result;
                }
                const obj = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = .{ .returnValue = result.? };
                return obj;
            },
            else => {
                std.log.warn("unknown statement...{any}", .{statement});
                // var obj: *object.Object = self.allocator.create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                return null;
            },
        }
    }

    pub fn eval_expression(self: *Self, expression: *ast.Expression, env: *environment.Environment) ?*object.Object {
        // std.log.warn("evaluating expression...{any}", .{expression});
        switch (expression.*) {
            .identifier => |i| {
                // std.log.warn("evaluating identifier...{any}", .{i});
                if (env.get(i.identifier)) |obj| {
                    // std.log.warn("found identifier...{any} for {s}", .{ obj, i.identifier });
                    return obj;
                }

                if (self.builtins.get(i.identifier)) |builtin| {
                    return builtin;
                }

                return new_error(self.arena.allocator(), "identifier not found: {s}", .{i.identifier});
            },
            .integer => |i| {
                const obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = .{ .integer = i };
                return obj;
            },
            .boolean => |b| {
                const obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = if (b) TRUE else FALSE;
                return obj;
            },
            .prefix => |p| {
                const right = self.eval_expression(p.right, env);
                if (is_error(right)) {
                    return right;
                }

                if (right) |r| {
                    return self.eval_prefix_expression(p.operator, r);
                } else {
                    return null;
                }
            },
            .infix => |i| {
                // std.log.warn("evaluating infix...{any}", .{i.left});
                // std.log.warn("evaluating infix...{any}", .{i});
                const left = self.eval_expression(i.left, env);
                if (is_error(left)) {
                    return left;
                }

                const right = self.eval_expression(i.right, env);
                if (is_error(right)) {
                    return right;
                }
                return self.eval_infix_expression(i.operator, left.?, right.?);
            },
            .ifExpression => |_| {
                return self.eval_if_expression(expression, env);
            },
            .functionLiteral => |fl| {
                const obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = .{ .function = .{ .parameters = fl.parameters.clone() catch unreachable, .body = fl.body, .env = env } };
                return obj;
            },
            .callExpression => |ce| {
                const function = self.eval_expression(ce.function, env);
                if (is_error(function)) {
                    return function;
                }

                const args = self.eval_expressions(ce.arguments, env);
                if (args.items.len == 1 and is_error(args.items[0])) {
                    return args.items[0];
                }

                return self.apply_function(function.?, args.items);
            },
            .stringLiteral => |s| {
                // std.log.warn("evaluating string literal...{s}", .{s});
                const obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = .{ .string = self.arena.allocator().dupe(u8, s) catch unreachable };
                return obj;
            },
            .arrayLiteral => |al| {
                const elements = self.eval_expressions(al.elements, env);
                if (elements.items.len == 1 and is_error(elements.items[0])) {
                    return elements.items[0];
                }
                const obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = .{ .array = elements };
                return obj;
            },
            .indexExpression => |ie| {
                const left = self.eval_expression(ie.left, env);
                if (is_error(left)) {
                    return left;
                }

                const index = self.eval_expression(ie.index, env);
                if (is_error(index)) {
                    return index;
                }

                return self.eval_index_expression(left.?, index.?);
            },
            .hashLiteral => |hl| {
                return self.eval_hash_literal(hl.pairs, env);
            },
            else => {
                return null;
            },
        }
    }

    fn eval_index_expression(self: *Self, left: *object.Object, index: *object.Object) ?*object.Object {
        if (left.* == .array and index.* == .integer) {
            return self.eval_array_index_expression(left, index);
        } else if (left.* == .hash) {
            return self.eval_hash_index_expression(left, index);
        } else {
            return new_error(self.arena.allocator(), "index operator not supported: {s}", .{left.typeId()});
        }
    }

    fn eval_hash_index_expression(self: *Self, hash: *object.Object, index: *object.Object) ?*object.Object {
        const key = index.hashKey();

        if (std.mem.eql(u8, key.type, "NULL")) {
            return new_error(self.arena.allocator(), "unusable as hash key: {s}", .{index.typeId()});
        }

        if (hash.hash.pairs.get(key)) |v| {
            return v.value;
        }

        const obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
        obj.* = .nil;
        return obj;
    }

    fn eval_array_index_expression(self: *Self, obj: *object.Object, index: *object.Object) ?*object.Object {
        const idx = index.intValue();
        const max = obj.array.items.len - 1;
        if (idx < 0 or idx > max) {
            const nil: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
            nil.* = .nil;
            return nil;
        }
        return obj.array.items[@as(usize, @intCast(idx))];
    }

    fn eval_hash_literal(self: *Self, hl: AutoHashMap(*ast.Expression, *ast.Expression), env: *environment.Environment) ?*object.Object {
        var pairs = std.HashMap(object.HashKey, object.HashPair, object.HashKeyContext, std.hash_map.default_max_load_percentage).init(self.arena.allocator());
        var it = hl.iterator();
        while (it.next()) |pair| {
            const key = self.eval_expression(pair.key_ptr.*, env);
            if (is_error(key)) {
                return key;
            }
            const value = self.eval_expression(pair.value_ptr.*, env);
            if (is_error(value)) {
                return value;
            }

            const hashPair = object.HashPair{ .key = key.?, .value = value.? };
            pairs.put(key.?.hashKey(), hashPair) catch unreachable;
        }
        const obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
        obj.* = .{ .hash = .{ .pairs = pairs } };
        return obj;
    }

    fn apply_function(self: *Self, fnObj: *object.Object, args: []*object.Object) ?*object.Object {
        switch (fnObj.*) {
            .function => |f| {
                const extendedEnv = self.extend_function_env(fnObj, args);
                const evaluated = self.eval_block_statement(f.body, extendedEnv);
                return unwrap_return_value(evaluated);
            },
            .builtin => |b| {
                return b(self.arena.allocator(), args);
            },
            else => {
                return new_error(self.arena.allocator(), "not a function: {s}", .{fnObj.typeId()});
            },
        }
    }

    fn extend_function_env(self: *Self, fnObj: *object.Object, args: []*object.Object) *environment.Environment {
        switch (fnObj.*) {
            .function => |f| {
                var extEnv = environment.Environment.initWithEnclosedEnv(self.arena.allocator(), f.env);
                for (f.parameters.items, 0..) |param, i| {
                    // std.log.warn("(extend env) setting {s} to {s}", .{ param.identifier, args[i] });
                    _ = extEnv.set(param.identifier, args[i]);
                }
                return extEnv;
            },
            else => {
                std.debug.panic("not a function: {s}", .{fnObj.typeId()});
            },
        }
    }

    fn unwrap_return_value(obj: ?*object.Object) ?*object.Object {
        if (obj) |o| {
            if (o.* == .returnValue) {
                return o.returnValue;
            }
            return o;
        }
        return null;
    }

    fn eval_expressions(self: *Self, exps: ArrayList(*ast.Expression), env: *environment.Environment) ArrayList(*object.Object) {
        var result = ArrayList(*object.Object).init(self.arena.allocator());
        for (exps.items) |e| {
            const evaluated = self.eval_expression(e, env);
            if (is_error(evaluated)) {
                result.append(evaluated.?) catch unreachable;
                return result;
            }
            if (evaluated) |ev| {
                result.append(ev) catch unreachable;
            }
        }
        return result;
    }

    fn eval_if_expression(self: *Self, ie: *ast.Expression, env: *environment.Environment) ?*object.Object {
        return switch (ie.*) {
            .ifExpression => |ifExpr| {
                const condition = self.eval_expression(ifExpr.condition, env);
                if (is_error(condition)) {
                    return condition;
                }

                if (condition.?.boolValue()) {
                    return self.eval_block_statement(ifExpr.consequence, env);
                } else if (ifExpr.alternative) |alternative| {
                    return self.eval_block_statement(alternative, env);
                } else {
                    const obj = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                    obj.* = .nil;
                    return obj;
                }
            },
            else => {
                return null;
            },
        };
    }

    fn eval_block_statement(self: *Self, bs: *ast.BlockStatement, env: *environment.Environment) ?*object.Object {
        var result: ?*object.Object = undefined;
        for (bs.statements.items) |stmt| {
            result = self.eval_statement(stmt, env);

            if (result) |r| {
                switch (r.*) {
                    .returnValue, .err => {
                        return result;
                    },
                    else => {},
                }
            }
        }
        return result;
    }

    pub fn eval_infix_expression(self: *Self, operator: []const u8, left: *object.Object, right: *object.Object) ?*object.Object {
        if (!std.mem.eql(u8, left.typeId(), right.typeId())) {
            return new_error(self.arena.allocator(), "type mismatch: {s} {s} {s}", .{ left.typeId(), operator, right.typeId() });
        }
        if (left.* == .integer and right.* == .integer) {
            return self.eval_integer_infix_expression(operator, left, right);
        } else if (left.* == .string and right.* == .string) {
            return self.eval_string_infix_expression(operator, left, right);
        } else if (std.mem.eql(u8, operator, "==")) {
            return self.eval_boolean_infix_expression(operator, left, right);
        } else if (std.mem.eql(u8, operator, "!=")) {
            return self.eval_boolean_infix_expression(operator, left, right);
        } else {
            return new_error(self.arena.allocator(), "unknown operator: {s} {s} {s}", .{ left.typeId(), operator, right.typeId() });
        }
    }

    pub fn eval_string_infix_expression(self: *Self, operator: []const u8, left: *object.Object, right: *object.Object) ?*object.Object {
        if (!std.mem.eql(u8, operator, "+")) {
            return new_error(self.arena.allocator(), "unknown operator: {s} {s} {s}", .{ left.typeId(), operator, right.typeId() });
        }
        const obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
        const str = std.fmt.allocPrint(self.arena.allocator(), "{s}{s}", .{ left.stringValue(), right.stringValue() }) catch unreachable;
        obj.* = .{ .string = str };
        return obj;
    }

    pub fn eval_boolean_infix_expression(self: *Self, operator: []const u8, left: *object.Object, right: *object.Object) ?*object.Object {
        const obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
        if (std.mem.eql(u8, operator, "==")) {
            obj.* = .{ .boolean = left.boolValue() == right.boolValue() };
            return obj;
        } else if (std.mem.eql(u8, operator, "!=")) {
            obj.* = .{ .boolean = left.boolValue() != right.boolValue() };
            return obj;
        } else {
            return null;
        }
    }

    pub fn eval_integer_infix_expression(self: *Self, operator: []const u8, left: *object.Object, right: *object.Object) ?*object.Object {
        const obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
        if (std.mem.eql(u8, operator, "+")) {
            obj.* = .{ .integer = left.intValue() + right.intValue() };
            return obj;
        } else if (std.mem.eql(u8, operator, "-")) {
            obj.* = .{ .integer = left.intValue() - right.intValue() };
            return obj;
        } else if (std.mem.eql(u8, operator, "*")) {
            obj.* = .{ .integer = left.intValue() * right.intValue() };
            return obj;
        } else if (std.mem.eql(u8, operator, "/")) {
            obj.* = .{ .integer = @divExact(left.intValue(), right.intValue()) };
            return obj;
        } else if (std.mem.eql(u8, operator, "<")) {
            obj.* = .{ .boolean = left.intValue() < right.intValue() };
            return obj;
        } else if (std.mem.eql(u8, operator, ">")) {
            obj.* = .{ .boolean = left.intValue() > right.intValue() };
            return obj;
        } else if (std.mem.eql(u8, operator, "==")) {
            obj.* = .{ .boolean = left.intValue() == right.intValue() };
            return obj;
        } else if (std.mem.eql(u8, operator, "!=")) {
            obj.* = .{ .boolean = left.intValue() != right.intValue() };
            return obj;
        } else {
            return new_error(self.arena.allocator(), "unknown operator: {s} {s} {s}", .{ left.typeId(), operator, right.typeId() });
        }
    }

    pub fn eval_prefix_expression(self: *Self, operator: []const u8, right: *object.Object) ?*object.Object {
        if (std.mem.eql(u8, operator, "!")) {
            return self.eval_bang_operator(right);
        } else if (std.mem.eql(u8, operator, "-")) {
            return self.eval_minus_prefix_operator(right);
        } else {
            return new_error(self.arena.allocator(), "unknown operator: {s}{s}", .{ operator, right.typeId() });
        }
    }

    pub fn eval_minus_prefix_operator(self: *Self, right: *object.Object) ?*object.Object {
        switch (right.*) {
            .integer => |i| {
                const obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = .{ .integer = -i };
                return obj;
            },
            else => {
                return new_error(self.arena.allocator(), "unknown operator: -{s}", .{right.typeId()});
            },
        }
    }

    pub fn eval_bang_operator(self: *Self, right: *object.Object) ?*object.Object {
        switch (right.*) {
            .boolean => |b| {
                const result = if (b) FALSE else TRUE;
                const obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = result;
                return obj;
            },
            .nil => {
                const obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = .{ .boolean = true };
                return obj;
            },
            else => {
                const obj: *object.Object = self.arena.allocator().create(object.Object) catch std.debug.panic("failed to allocate object", .{});
                obj.* = .{ .boolean = false };
                return obj;
            },
        }
    }

    pub fn is_error(obj: ?*object.Object) bool {
        if (obj) |o| {
            return o.* == .err;
        }
        return false;
    }
};

const assert = std.debug.assert;
var test_allocator = std.testing.allocator;

fn test_eval(allocator: std.mem.Allocator, evaluator: *Evaluator, input: []const u8) *object.Object {
    // std.log.warn("input: {s}\n", .{input});
    var l = lexer.Lexer.init(allocator, input);
    defer l.deinit();

    var p = parser.Parser.init(l, allocator);
    defer p.deinit();
    const prog = p.parseProgram();
    const node = .{ .program = prog };
    // std.log.warn("node: {any}\n", .{node});
    // std.log.warn("evaluator: {any}\n", .{evaluator});
    var env = environment.Environment.init(test_allocator);
    defer env.deinit();
    return evaluator.eval(node, env) orelse std.debug.panic("failed to evaluate", .{});
}

// fn test_eval2(allocator: std.mem.Allocator, evaluator: *Evaluator, input: []const u8) *object.Object {
//     // std.log.warn("input: {s}\n", .{input});
//     var l = lexer.Lexer.init(input);
//     var p = parser.Parser.init(l, allocator);
//     defer p.deinit();
//     var prog = p.parseProgram();
//     var node = .{ .program = &prog };
//     // std.log.warn("node: {any}\n", .{node});
//     // std.log.warn("evaluator: {any}\n", .{evaluator});
//     var env = environment.Environment.init(test_allocator);
//     defer env.deinit();
//     return evaluator.eval(node, &env) orelse std.debug.panic("failed to evaluate", .{});
// }

fn test_integer_object(obj: *object.Object, expected: i64) void {
    switch (obj.*) {
        .integer => |i| {
            // std.log.warn("{d} == {d}\n", .{ i, expected });
            assert(i == expected);
        },
        else => {
            std.log.warn("test_integer_object: object is not Integer. got={s}\n", .{obj});
        },
    }
}

fn test_boolean_object(obj: *object.Object, expected: bool) void {
    switch (obj.*) {
        .boolean => |b| {
            assert(b == expected);
        },
        else => {
            std.log.warn("object is not Boolean. got={s}\n", .{obj});
        },
    }
}

test "test eval integer expression" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "5", .expected = 5 },
        .{ .input = "10", .expected = 10 },
        .{ .input = "-5", .expected = -5 },
        .{ .input = "-10", .expected = -10 },
        .{ .input = "5 + 5 + 5 + 5 - 10", .expected = 10 },
        .{ .input = "2 * 2 * 2 * 2 * 2", .expected = 32 },
        .{ .input = "-50 + 100 + -50", .expected = 0 },
        .{ .input = "5 * 2 + 10", .expected = 20 },
        .{ .input = "5 + 2 * 10", .expected = 25 },
        .{ .input = "20 + 2 * -10", .expected = 0 },
        .{ .input = "50 / 2 * 2 + 10", .expected = 60 },
        .{ .input = "2 * (5 + 10)", .expected = 30 },
        .{ .input = "3 * 3 * 3 + 10", .expected = 37 },
        .{ .input = "3 * (3 * 3) + 10", .expected = 37 },
        .{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .expected = 50 },
    };

    // std.log.warn("tests: {d}\n", .{tests.len});

    for (tests) |t| {
        // std.log.warn("input: {s}\n", .{t.input});
        var evaluator = Evaluator.init(test_allocator);
        const o = test_eval(test_allocator, &evaluator, t.input);
        defer evaluator.deinit();
        // std.log.warn("o: {s}\n", .{o});
        // _ = test_eval(t.input);
        test_integer_object(o, t.expected);
    }
}

test "test eval boolean expression" {
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "true", .expected = true },
        .{ .input = "false", .expected = false },
        .{ .input = "1 < 2", .expected = true },
        .{ .input = "1 > 2", .expected = false },
        .{ .input = "1 < 1", .expected = false },
        .{ .input = "1 > 1", .expected = false },
        .{ .input = "1 == 1", .expected = true },
        .{ .input = "1 != 1", .expected = false },
        .{ .input = "1 == 2", .expected = false },
        .{ .input = "1 != 2", .expected = true },
        .{ .input = "true == true", .expected = true },
        .{ .input = "false == false", .expected = true },
        .{ .input = "true == false", .expected = false },
        .{ .input = "true != false", .expected = true },
        .{ .input = "false != true", .expected = true },
        .{ .input = "(1 < 2) == true", .expected = true },
        .{ .input = "(1 < 2) == false", .expected = false },
        .{ .input = "(1 > 2) == true", .expected = false },
        .{ .input = "(1 > 2) == false", .expected = true },
    };

    for (tests) |t| {
        var evaluator = Evaluator.init(test_allocator);
        const o = test_eval(test_allocator, &evaluator, t.input);
        defer evaluator.deinit();
        // std.log.warn("o: {s}\n", .{o});
        // _ = test_eval(t.input);
        test_boolean_object(o, t.expected);
    }
}

test "test bang operator" {
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "!true", .expected = false },
        .{ .input = "!false", .expected = true },
        .{ .input = "!5", .expected = false },
        .{ .input = "!!true", .expected = true },
        .{ .input = "!!false", .expected = false },
        .{ .input = "!!5", .expected = true },
    };

    for (tests) |t| {
        var evaluator = Evaluator.init(test_allocator);
        const o = test_eval(test_allocator, &evaluator, t.input);
        defer evaluator.deinit();
        test_boolean_object(o, t.expected);
    }
}

const val = union(enum) {
    integer: i64,
    string: []const u8,
    integerArray: []const i64,
    nil,
};

test "test if else expressions" {
    const tests = [_]struct {
        input: []const u8,
        expected: val,
    }{
        .{ .input = "if (true) { 10 }", .expected = .{ .integer = 10 } },
        .{ .input = "if (false) { 10 }", .expected = .nil },
        .{ .input = "if (1) { 10 }", .expected = .{ .integer = 10 } },
        .{ .input = "if (1 < 2) { 10 }", .expected = .{ .integer = 10 } },
        .{ .input = "if (1 > 2) { 10 }", .expected = .nil },
        .{ .input = "if (1 > 2) { 10 } else { 20 }", .expected = .{ .integer = 20 } },
        .{ .input = "if (1 < 2) { 10 } else { 20 }", .expected = .{ .integer = 10 } },
    };

    for (tests) |t| {
        var evaluator = Evaluator.init(test_allocator);
        const o = test_eval(test_allocator, &evaluator, t.input);
        defer evaluator.deinit();
        switch (t.expected) {
            .integer => {
                test_integer_object(o, t.expected.integer);
            },
            .nil => {
                assert(o.* == .nil);
            },
            else => {},
        }
    }
}

test "test return statements" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "return 10;", .expected = 10 },
        .{ .input = "return 10; 9;", .expected = 10 },
        .{ .input = "return 2 * 5; 9;", .expected = 10 },
        .{ .input = "9; return 2 * 5; 9;", .expected = 10 },
        .{ .input = "if (10 > 1) { if (10 > 1) { return 10; } return 1; }", .expected = 10 },
    };

    for (tests) |t| {
        var evaluator = Evaluator.init(test_allocator);
        const o = test_eval(test_allocator, &evaluator, t.input);
        defer evaluator.deinit();
        test_integer_object(o, t.expected);
    }
}

test "test error handling" {
    const tests = [_]struct {
        input: []const u8,
        expected: []const u8,
    }{
        .{ .input = "5 + true;", .expected = "type mismatch: INTEGER + BOOLEAN" },
        .{ .input = "5 + true; 5;", .expected = "type mismatch: INTEGER + BOOLEAN" },
        .{ .input = "-true", .expected = "unknown operator: -BOOLEAN" },
        .{ .input = "true + false;", .expected = "unknown operator: BOOLEAN + BOOLEAN" },
        .{ .input = "5; true + false; 5", .expected = "unknown operator: BOOLEAN + BOOLEAN" },
        .{ .input = "if (10 > 1) { true + false; }", .expected = "unknown operator: BOOLEAN + BOOLEAN" },
        .{ .input = "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }", .expected = "unknown operator: BOOLEAN + BOOLEAN" },
        .{ .input = "foobar", .expected = "identifier not found: foobar" },
        .{ .input = "\"hello\" - \"world\"", .expected = "unknown operator: STRING - STRING" },
        .{ .input = "{\"name\": \"Monkey\"}[fn(x) { x }];", .expected = "unusable as hash key: FUNCTION" },
    };

    for (tests) |t| {
        var evaluator = Evaluator.init(test_allocator);
        const o = test_eval(test_allocator, &evaluator, t.input);
        defer evaluator.deinit();
        // std.log.warn("actual: {s}\n", .{o});
        // std.log.warn("expected: {s}\n", .{t.expected});
        switch (o.*) {
            .err => |e| {
                // std.log.warn("o: {s}\n", .{o});
                assert(std.mem.eql(u8, e, t.expected));
            },
            else => {
                std.debug.panic("no error object returned", .{});
            },
        }
    }
}

test "test let statement" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "let a = 5; a;", .expected = 5 },
        .{ .input = "let a = 5 * 5; a;", .expected = 25 },
        .{ .input = "let a = 5; let b = a; b;", .expected = 5 },
        .{ .input = "let a = 5; let b = a; let c = a + b + 5; c;", .expected = 15 },
    };

    for (tests) |t| {
        var evaluator = Evaluator.init(test_allocator);
        const o = test_eval(test_allocator, &evaluator, t.input);
        defer evaluator.deinit();
        test_integer_object(o, t.expected);
    }
}

test "test function object" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    const input = "fn(x) { x + 2; }";
    var evaluator = Evaluator.init(allocator);
    defer evaluator.deinit();
    const o = test_eval(allocator, &evaluator, input);
    switch (o.*) {
        .function => |f| {
            assert(f.parameters.items.len == 1);
            const param = f.parameters.items[0];
            // std.log.warn("f.parameters.items[0].identifier: {any}\n", .{param});
            // std.log.warn("f.parameters.items[0].identifier: {any}\n", .{f.parameters.items});
            assert(std.mem.eql(u8, param.identifier, "x"));
            const body = std.fmt.allocPrint(test_allocator, "{s}", .{f.body}) catch unreachable;
            defer test_allocator.free(body);
            // std.log.warn("body: {s}\n", .{body});
            assert(std.mem.eql(u8, body, "(x + 2)"));
        },
        else => {
            std.debug.panic("object is not Function. got={s}", .{o});
        },
    }
}

test "test function application" {
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "let identity = fn(x) { x; }; identity(5);", .expected = 5 },
        .{ .input = "let identity = fn(x) { return x; }; identity(5);", .expected = 5 },
        .{ .input = "let double = fn(x) { x * 2; }; double(5);", .expected = 10 },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5, 5);", .expected = 10 },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", .expected = 20 },
        .{ .input = "fn(x) { x; }(5)", .expected = 5 },
    };

    for (tests) |t| {
        var evaluator = Evaluator.init(allocator);
        const o = test_eval(allocator, &evaluator, t.input);
        defer evaluator.deinit();
        test_integer_object(o, t.expected);
    }
}

test "test closures" {
    const input = "let newAdder = fn(x) { fn(y) { x + y }; }; let addTwo = newAdder(2); addTwo(2);";
    const expected = 4;

    var evaluator = Evaluator.init(test_allocator);
    const o = test_eval(test_allocator, &evaluator, input);
    defer evaluator.deinit();
    test_integer_object(o, expected);
}

test "test string literal" {
    const input = "\"hello world\"";
    const expected = "hello world";

    var evaluator = Evaluator.init(test_allocator);
    const o = test_eval(test_allocator, &evaluator, input);
    defer evaluator.deinit();
    switch (o.*) {
        .string => |s| {
            assert(std.mem.eql(u8, s, expected));
        },
        else => {
            std.debug.panic("object is not String. got={s}", .{o});
        },
    }
}

test "test string concatenation" {
    const input = "\"hello\" + \" \" + \"world\"";
    const expected = "hello world";

    var evaluator = Evaluator.init(test_allocator);
    const o = test_eval(test_allocator, &evaluator, input);
    defer evaluator.deinit();
    switch (o.*) {
        .string => |s| {
            assert(std.mem.eql(u8, s, expected));
        },
        else => {
            std.debug.panic("object is not String. got={s}", .{o});
        },
    }
}

test "test builtin functions" {
    // const restArray = [_]i64{ 2, 3 };
    const tests = [_]struct {
        input: []const u8,
        expected: val,
    }{
        .{ .input = "len(\"\")", .expected = .{ .integer = 0 } },
        .{ .input = "len(\"four\")", .expected = .{ .integer = 4 } },
        .{ .input = "len(\"hello world\")", .expected = .{ .integer = 11 } },
        .{ .input = "len(1)", .expected = .{ .string = "argument to `len` not supported, got INTEGER" } },
        .{ .input = "len(\"one\", \"two\")", .expected = .{ .string = "wrong number of arguments. got=2, want=1" } },
        .{ .input = "len([1, 2, 3])", .expected = .{ .integer = 3 } },
        .{ .input = "len([])", .expected = .{ .integer = 0 } },
        .{ .input = "first([1, 2, 3])", .expected = .{ .integer = 1 } },
        .{ .input = "first([])", .expected = .nil },
        .{ .input = "first(1)", .expected = .{ .string = "argument to `first` must be ARRAY, got INTEGER" } },
        .{ .input = "last([1, 2, 3])", .expected = .{ .integer = 3 } },
        .{ .input = "last([])", .expected = .nil },
        .{ .input = "last(1)", .expected = .{ .string = "argument to `last` must be ARRAY, got INTEGER" } },
        .{ .input = "rest([1, 2, 3])", .expected = .{ .integerArray = &[_]i64{ 2, 3 } } },
        .{ .input = "rest([])", .expected = .nil },
        .{ .input = "push([], 1)", .expected = .{ .integerArray = &[_]i64{1} } },
        .{ .input = "push(1, 1)", .expected = .{ .string = "argument to `push` must be ARRAY, got INTEGER" } },
    };

    for (tests) |t| {
        var evaluator = Evaluator.init(test_allocator);
        const o = test_eval(test_allocator, &evaluator, t.input);
        defer evaluator.deinit();
        switch (t.expected) {
            .integer => {
                test_integer_object(o, t.expected.integer);
            },
            .string => {
                switch (o.*) {
                    .string => |s| {
                        assert(std.mem.eql(u8, s, t.expected.string));
                    },
                    .err => |e| {
                        // std.log.warn("expected: {s}, actual: {s}\n", .{ t.expected.string, e });
                        assert(std.mem.eql(u8, e, t.expected.string));
                    },
                    else => {
                        std.debug.panic("object is not String. got={s}", .{o});
                    },
                }
            },
            .integerArray => {
                switch (o.*) {
                    .array => |a| {
                        assert(a.items.len == t.expected.integerArray.len);
                        for (a.items, t.expected.integerArray) |elem, exp| {
                            test_integer_object(elem, exp);
                        }
                    },
                    else => {
                        std.debug.panic("object is not Array. got={s}", .{o});
                    },
                }
            },
            .nil => {
                assert(o.* == .nil);
            },
        }
    }
}

test "test array literal" {
    const input = "[1, 2 * 2, 3 + 3]";
    const expected = [_]i64{ 1, 4, 6 };

    var evaluator = Evaluator.init(test_allocator);
    const o = test_eval(test_allocator, &evaluator, input);
    defer evaluator.deinit();
    switch (o.*) {
        .array => |a| {
            assert(a.items.len == expected.len);
            for (a.items, expected) |elem, exp| {
                test_integer_object(elem, exp);
            }
        },
        else => {
            std.debug.panic("object is not Array. got={s}", .{o});
        },
    }
}

test "test array index expressions" {
    const tests = [_]struct {
        input: []const u8,
        expected: val,
    }{
        .{ .input = "[1, 2, 3][0]", .expected = .{ .integer = 1 } },
        .{ .input = "[1, 2, 3][1]", .expected = .{ .integer = 2 } },
        .{ .input = "[1, 2, 3][2]", .expected = .{ .integer = 3 } },
        .{ .input = "let i = 0; [1][i];", .expected = .{ .integer = 1 } },
        .{ .input = "[1, 2, 3][1 + 1];", .expected = .{ .integer = 3 } },
        .{ .input = "let myArray = [1, 2, 3]; myArray[2];", .expected = .{ .integer = 3 } },
        .{ .input = "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", .expected = .{ .integer = 6 } },
        .{ .input = "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", .expected = .{ .integer = 2 } },
        .{ .input = "[1, 2, 3][3]", .expected = .nil },
        .{ .input = "[1, 2, 3][-1]", .expected = .nil },
    };

    for (tests) |t| {
        var evaluator = Evaluator.init(test_allocator);
        const o = test_eval(test_allocator, &evaluator, t.input);
        defer evaluator.deinit();
        switch (t.expected) {
            .integer => {
                test_integer_object(o, t.expected.integer);
            },
            .nil => {
                assert(o.* == .nil);
            },
            else => {
                std.debug.panic("object is not Integer. got={s}", .{o});
            },
        }
    }
}

test "test hash literals" {
    const input =
        \\let two = "two";
        \\{
        \\    "one": 10 - 9,
        \\    two: 1 + 1,
        \\    "thr" + "ee": 6 / 2,
        \\    4: 4,
        \\    true: 5,
        \\    false: 6
        \\};
    ;

    var expected = std.HashMap(object.HashKey, i64, object.HashKeyContext, std.hash_map.default_max_load_percentage).init(test_allocator);
    defer expected.deinit();
    var oo = object.Object{ .string = "one" };
    try expected.put(oo.hashKey(), 1);
    oo = object.Object{ .string = "two" };
    try expected.put(oo.hashKey(), 2);
    oo = object.Object{ .string = "three" };
    try expected.put(oo.hashKey(), 3);
    oo = object.Object{ .integer = 4 };
    try expected.put(oo.hashKey(), 4);
    try expected.put(TRUE.hashKey(), 5);
    try expected.put(FALSE.hashKey(), 6);

    var evaluator = Evaluator.init(test_allocator);
    const o = test_eval(test_allocator, &evaluator, input);
    defer evaluator.deinit();
    switch (o.*) {
        .hash => |h| {
            assert(h.pairs.count() == expected.count());
            var it = expected.iterator();
            while (it.next()) |pair| {
                const expectedValue = expected.get(pair.key_ptr.*);
                const actualValue = h.pairs.get(pair.key_ptr.*);
                test_integer_object(actualValue.?.value, expectedValue.?);
            }
        },
        else => {
            std.debug.panic("object is not Hash. got={s}", .{o});
        },
    }
}

test "test hash index expressions" {
    const tests = [_]struct {
        input: []const u8,
        expected: val,
    }{
        .{ .input = "{\"foo\": 5}[\"foo\"]", .expected = .{ .integer = 5 } },
        .{ .input = "{\"foo\": 5}[\"bar\"]", .expected = .nil },
        .{ .input = "let key = \"foo\"; {\"foo\": 5}[key]", .expected = .{ .integer = 5 } },
        .{ .input = "{}[\"foo\"]", .expected = .nil },
        .{ .input = "{5: 5}[5]", .expected = .{ .integer = 5 } },
        .{ .input = "{true: 5}[true]", .expected = .{ .integer = 5 } },
        .{ .input = "{false: 5}[false]", .expected = .{ .integer = 5 } },
    };

    for (tests) |t| {
        var evaluator = Evaluator.init(test_allocator);
        const o = test_eval(test_allocator, &evaluator, t.input);
        defer evaluator.deinit();
        switch (t.expected) {
            .integer => {
                test_integer_object(o, t.expected.integer);
            },
            .nil => {
                assert(o.* == .nil);
            },
            else => {
                std.debug.panic("object is not Integer. got={s}", .{o});
            },
        }
    }
}

// test "memory test" {
//     var gpa = std.testing.allocator;

//     var arena = std.heap.ArenaAllocator.init(gpa);
//     defer arena.deinit();

//     var env = environment.Environment.init(arena.allocator());
//     defer env.deinit();
//     // defer _ = gpa.deinit();

//     var lines = ArrayList([]const u8).init(gpa);
//     try lines.append("let addTwo = fn(x) { x + 2; };");
//     try lines.append("let result = addTwo(2);");
//     try lines.append("result;");
//     defer lines.deinit();

//     for (lines.items) |line| {
//         var l = lexer.Lexer.init(arena.allocator(), line);
//         defer l.deinit();

//         var p = parser.Parser.init(l, arena.allocator());
//         defer p.deinit();

//         var prog = p.parseProgram();

//         if (p.errors.items.len > 0) {
//             printParserErrors(p.errors);
//             continue;
//         }

//         var node = .{ .program = &prog };

//         var e = Evaluator.init(arena.allocator());
//         defer e.deinit();

//         var evaluated = e.eval(node, env);

//         if (evaluated) |result| {
//             std.log.warn("Evaled: {s}\n", .{result});
//         }
//     }
// }

// fn printParserErrors(errors: ArrayList([]u8)) void {
//     std.debug.print("Woops! We ran into some monkey business here!\n", .{});
//     std.debug.print(" parser errors:\n", .{});
//     for (errors.items) |err| {
//         std.debug.print("\t{s}\n", .{err});
//     }
// }
