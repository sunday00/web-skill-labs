const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

const he = "Hello";
const wd = "World";

const Error = error{ Boom, OutOfMemory };

pub fn run() !void {
    const Pool = std.heap.MemoryPool;

    var pool = Pool(u32).init(std.heap.page_allocator);
    defer pool.deinit();

    const p1 = try pool.create();
    const p2 = try pool.create();
    const p3 = try pool.create();

    u.print("{any} {any} {any}", .{ p1, p2, p3 });
    //                                             ^
    pool.destroy(p2); //                           |
    //                                             |
    const p4 = try pool.create(); //               |
    u.print("{any}", .{p4}); // <---- === reuse destroyed
}
