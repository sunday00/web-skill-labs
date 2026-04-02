const std = @import("std");
const builtin = @import("builtin");

const Allocator = std.mem.Allocator;

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

pub fn AList(comptime T: type) type {
    return struct {
        const Node = struct {
            data: T,
            next: ?*Node,

            fn init(allocator: Allocator, data: T) !*Node {
                const ptr = try allocator.create(Node);
                // ptr.allocator = allocator;
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
