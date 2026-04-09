const std = @import("std");

fn work(id: usize) void {
    std.debug.print("{} started \n", .{id});
    std.Thread.sleep(500 * 1000 * 1000);
    std.debug.print("{} finished \n", .{id});
}

fn noThreading() !void {
    const cpus = try std.Thread.getCpuCount();

    for (0..cpus) |i| {
        work(i);
    }
}

fn threadingWithJoin(alloc: std.mem.Allocator) !std.ArrayList(std.Thread) {
    const cpus = try std.Thread.getCpuCount();
    var handlers: std.ArrayList(std.Thread) = .empty;

    for (0..cpus) |i| {
        const handler = try std.Thread.spawn(.{}, work, .{i});

        try handlers.append(alloc, handler);
    }

    for (handlers.items) |h| h.join();

    return handlers;
}

fn threadingWithDetach() !void {
    const cpus = try std.Thread.getCpuCount();

    for (0..cpus) |i| {
        var handler = try std.Thread.spawn(.{}, work, .{i});
        handler.detach();
    }

    std.Thread.sleep(500 * std.time.ns_per_ms);
}

pub fn main() !void {

    // try noThreading();

    // var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    // defer _ = gpa.deinit();
    // const alloc = gpa.allocator();
    // var handlers = try threadingWithJoin(alloc);
    // defer handlers.deinit(alloc);

    try threadingWithDetach();
}
