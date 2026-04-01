const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

const Allocator = std.mem.Allocator;

const he = "Hello";
const wd = "World";

const Error = error{ Boom, OutOfMemory };

pub fn List(comptime T: type) type {
    return struct {
        const Node = struct {
            allocator: Allocator,
            data: T,
            next: ?*Node,

            fn init(allocator: Allocator, data: T) !*Node {
                const ptr = try allocator.create(Node);
                ptr.allocator = allocator;
                ptr.data = data;
                ptr.next = null;

                return ptr;
            }

            fn deinit(self: *Node) void {
                if (self.next) |ptr| ptr.deinit();
                self.allocator.destroy(self);
            }
        };

        const Self = @This();

        allocator: Allocator,
        head: *Node,

        pub fn init(allocator: Allocator, data: T) !Self {
            return .{
                .allocator = allocator,
                .head = try Node.init(allocator, data),
            };
        }

        pub fn deinit(self: *Self) void {
            self.head.deinit();
        }

        pub fn append(self: *Self, data: T) !void {
            var tail: *Node = self.head;
            while (tail.next) |ptr| tail = ptr;

            tail.next = try Node.init(self.allocator, data);
        }

        pub fn lookup(self: Self, data: T) bool {
            var current: ?*Node = self.head;

            return while (current) |node_ptr| {
                if (node_ptr.data == data) break true;

                current = node_ptr.next;
            } else false;
        }
    };
}

fn runGPAList() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    const iter: usize = 100;
    const item_count: usize = 1_000;

    var timer = try std.time.Timer.start();

    for (0..iter) |_| {
        var list = try List(usize).init(allocator, 13);
        errdefer list.deinit();

        for (0..item_count) |i| try list.append(i);

        list.deinit();
    }

    var took: f64 = @floatFromInt(timer.read());
    took /= std.time.ns_per_ms;

    u.print("took {d:.2}ms", .{took});
}

pub fn AList(comptime T: type) type {
    return struct {
        const Node = struct {
            data: T,
            next: ?*Node,

            fn init(allocator: Allocator, data: T) !*Node {
                const ptr = try allocator.create(Node);
                ptr.allocator = allocator;
                ptr.data = data;
                ptr.next = null;

                return ptr;
            }
        };

        const Self = @This();

        arena: std.heap.ArenaAllocator,
        head: *Node,

        pub fn init(allocator: Allocator, data: T) !Self {
            var l = Self{
                .arena = std.heap.ArenaAllocator.init(allocator),
                .head = undefined,
            };

            l.head = try Node.init(l.arena.allocator(), data);

            return l;
        }

        pub fn deinit(self: *Self) void {
            self.arena.deinit();
        }

        pub fn append(self: *Self, data: T) !void {
            var tail: *Node = self.head;
            while (tail.next) |ptr| tail = ptr;

            tail.next = try Node.init(self.arena.allocator(), data);
        }

        pub fn lookup(self: Self, data: T) bool {
            var current: ?*Node = self.head;

            return while (current) |node_ptr| {
                if (node_ptr.data == data) break true;

                current = node_ptr.next;
            } else false;
        }
    };
}

fn runAList() !void {
    const allocator = std.heap.page_allocator;

    const iter: usize = 100;
    const item_count: usize = 1_000;

    var timer = try std.time.Timer.start();

    for (0..iter) |_| {
        var list = try List(usize).init(allocator, 13);
        errdefer list.deinit();

        for (0..item_count) |i| try list.append(i);

        list.deinit();
    }

    var took: f64 = @floatFromInt(timer.read());
    took /= std.time.ns_per_ms;

    u.print("took {d:.2}ms", .{took});
}

pub fn run() !void {
    // try runGPAList();
    try runAList();
}
