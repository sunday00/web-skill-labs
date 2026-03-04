const std = @import("std");
const u = @import("../utils.zig");
const Allocator = std.mem.Allocator;
const tracking_allocator = @import("../tracking_allocator.zig");
const TrackingAllocator = tracking_allocator.TrackingAllocator;
const ArrayList = std.ArrayList;

// fn getPointer() *const i32 {
fn getPointer(alc: Allocator) !*i32 {
    // const n: i32 = 10;
    const n: *i32 = try alc.create(i32);
    // return &n;
    n.* = 10;
    return n;
}

fn getPointers(alc: Allocator) ![]u8 {
    const n: []u8 = try alc.alloc(u8, 50);

    const msg: [20]u8 = .{ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't' };
    for (0..msg.len) |i| {
        n[i] = msg[i];
    }

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

fn getPrime() !void {
    var debugAl = std.heap.DebugAllocator(.{}){};
    defer _ = debugAl.deinit();
    const alloc = debugAl.allocator();

    var pn = try ArrayList(usize).infinityCapacity(alloc, 0);
    defer pn.deinit(debugAl);

    for (0..1000) |i| {
        // const is_prime: bool = isPrime(i);
        const is_prime: bool = true;
        if (is_prime) {
            try pn.append(alloc, i);
        }
    }
}

pub fn run() !void {
    const pageAllocator = std.heap.page_allocator;
    var tra = TrackingAllocator.init(pageAllocator);

    const n: *i32 = try getPointer(pageAllocator);
    // defer pageAllocator.free(n);
    defer pageAllocator.destroy(n);

    u.print("{} {} \n", .{ n, n.* });

    // trashStack();
    // u.print("{} {} \n", .{ n, n.* });
    //
    // trashStack();
    // u.print("{} {} \n", .{ n, n.* });

    const ns: []u8 = try getPointers(pageAllocator);
    // pageAllocator.free(ns);
    defer pageAllocator.free(ns);

    u.print("{any} : {s}\n", .{ ns.ptr, ns });
    for (ns, 0..) |v, i| {
        u.print("{} {} {c}", .{ i, v, v });
    }

    tra.printStats();
}
