const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");
const lib = @import("../lib.zig");

fn methods(comptime T: type) type {
    return struct {
        fn printCharList(l: std.ArrayList(T)) !void {
            std.debug.print("check inside: [", .{});

            for (l.items) |value| {
                std.debug.print("{c} ", .{value});
            }

            std.debug.print("] \n\n", .{});
        }

        fn basic(alloc: std.mem.Allocator) !std.ArrayList(T) {
            var l: std.ArrayList(T) = .empty;

            // try l.append(alloc, "hello");
            for ("hello, World!") |byte| try l.append(alloc, byte);

            return l;
        }

        fn compact(alloc: std.mem.Allocator, cap: usize) !std.ArrayList(T) {
            var l: std.ArrayList(T) = try std.ArrayList(T).initCapacity(alloc, cap);

            for ("hello, World!") |byte| l.appendAssumeCapacity(byte);

            return l;
        }

        fn usingWriter(alloc: std.mem.Allocator, l: *std.ArrayList(T)) !void {
            const w = l.writer(alloc);
            try w.print("?, {s}", .{"Huh?"});
        }

        fn usingPopIter(l: *std.ArrayList(T)) !void {
            while (l.pop()) |b| {
                std.debug.print("{c}", .{b});
            }

            std.debug.print("\n\n", .{});
        }

        fn addpendSlice(alloc: std.mem.Allocator, l: *std.ArrayList(T)) !void {
            try l.appendSlice(alloc, "Another greet. Say Hi again");
        }

        fn removeByIndex(l: *std.ArrayList(T), i: usize) !T {
            return l.orderedRemove(i);
        }

        fn swapRemove(l: *std.ArrayList(T), i: usize) !T {
            // swap remove
            // remove i
            // remove tail
            // set i to tail.
            //
            // ex: [a, b, c, d, e]
            // list.swapRemove(2)
            // // remain [a. b, d, e] and throw away c
            // // [a, b, d] and last is e
            // // [a, b, e, d] is done. e is now located originally c idx.

            return l.swapRemove(i);
        }

        fn ownedSlice(alloc: std.mem.Allocator, l: *std.ArrayList(T)) ![]T {
            // std.ArrayList to empty.
            // convert to new slice
            //
            //⚠️ new slice should free on somewhere.
            return l.toOwnedSlice(alloc);
        }
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const m = methods(u8);

    var l1 = try m.basic(alloc);
    defer l1.deinit(alloc);

    try m.printCharList(l1);
    u.print("len: {} cap: {}", .{ l1.items.len, l1.capacity });

    _ = l1.pop();

    try m.printCharList(l1);

    try m.usingWriter(alloc, &l1);

    try m.printCharList(l1);

    try m.usingPopIter(&l1);

    try m.printCharList(l1);

    try m.addpendSlice(alloc, &l1);

    try m.printCharList(l1);

    const removed = try m.removeByIndex(&l1, 2);
    u.print("{c}", .{removed});
    try m.printCharList(l1);

    const removed2 = try m.swapRemove(&l1, 2);
    u.print("{c}", .{removed2});
    try m.printCharList(l1);

    l1.items[2] = 'X'; // <--- directly insert possible.
    try m.printCharList(l1);

    const s = try m.ownedSlice(alloc, &l1);
    defer alloc.free(s);
    u.print("{any}", .{s});
    try m.printCharList(l1);

    u.print("==-=-=-=-===--=-=-===-=-======-=-=---=", .{});

    var l2 = try m.compact(alloc, 20);
    defer l2.deinit(alloc);
    try m.printCharList(l2);
    u.print("len: {} cap: {}", .{ l2.items.len, l2.capacity });
}
