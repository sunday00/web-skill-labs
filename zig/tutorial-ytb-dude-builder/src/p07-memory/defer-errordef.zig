const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

const Allocator = std.mem.Allocator;

const List = struct {
    alloc: Allocator,
    head: *Node,
    tail: *Node,

    const Node = struct {
        data: u8,
        next: ?*Node,

        fn create(alloc: Allocator, data: u8) !*Node {
            var self = try alloc.create(Node);
            self.data = data;
            self.next = null;

            return self;
        }

        fn deinit(self: *Node, alloc: Allocator) void {
            if (self.next) |node_ptr| {
                node_ptr.deinit(alloc);
                alloc.destroy(node_ptr);
            }
        }
    };

    fn init(alloc: Allocator, data: u8) !List {
        var self = List{
            .alloc = alloc,
            .head = try Node.create(alloc, data),
            .tail = undefined,
        };

        self.tail = self.head;

        return self;
    }

    fn initWithSlice(alloc: Allocator, slice: []const u8) !List {
        var self = try List.init(alloc, slice[0]);

        errdefer self.deinit();

        for (slice[1..]) |data| try self.append(data);
        return self;
    }

    fn deinit(self: *List) void {
        self.head.deinit(self.alloc);
        self.alloc.destroy(self.head);
    }

    fn append(self: *List, data: u8) !void {
        self.tail.next = try Node.create(self.alloc, data);
        self.tail = self.tail.next.?;
    }

    fn contains(self: List, data: u8) bool {
        var current: ?*Node = self.head;

        return while (current) |node_ptr| {
            if (node_ptr.data == data) break true;
            current = node_ptr.next;
        } else false;
    }

    fn print(self: List) void {
        var current: ?*Node = self.head;
        var i: usize = 0;

        while (current) |node_ptr| : (i += 1) {
            std.debug.print("{} : {}\n", .{ i, node_ptr.data });
            current = node_ptr.next;
        }
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();

    const alloc = gpa.allocator();

    var list = try List.initWithSlice(alloc, &.{ 13, 42, 33 });
    defer list.deinit();

    try list.append(99);

    std.debug.print("42?: {}\n", .{list.contains(42)});
    std.debug.print("99?: {}\n", .{list.contains(99)});
    std.debug.print("100?: {}\n", .{list.contains(100)});

    list.print();

    errdefer |err| std.debug.print("errrr {}\n", .{err});

    // return error.Boom;
}
