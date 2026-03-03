const std = @import("std");
const u = @import("../utils.zig");
const Allocator = std.mem.Allocator;

// fn getPointer() *const i32 {
fn getPointer(alc: Allocator) !*i32 {
    // const n: i32 = 10;
    const n: *i32 = try alc.create(i32);
    // return &n;
    n.* = 10;
    return n;
}

fn trashStack() void {
    var buf: [4096]u8 = undefined;
    for (0..4096) |i| {
        buf[i] = 0;
    }
}

fn printArray() void {
    const arr = [_]i32{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    for (0..10) |i| {
        const v: i32 = arr[i];
        u.print("{}: {}", .{ i, v });
    }
}

pub fn run() !void {
    const pageAllocator = std.heap.page_allocator;

    const n: *i32 = try getPointer(pageAllocator);
    // defer pageAllocator.free(n);
    defer pageAllocator.destroy(n);

    u.print("{} {} \n", .{ n, n.* });

    // trashStack();
    // u.print("{} {} \n", .{ n, n.* });
    //
    // trashStack();
    // u.print("{} {} \n", .{ n, n.* });
}
