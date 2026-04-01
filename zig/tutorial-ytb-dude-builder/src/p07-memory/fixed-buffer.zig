const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

const Allocator = std.mem.Allocator;

const he = "Hello";
const wd = "World";

const Error = error{ Boom, OutOfMemory };

fn catAlloc(allocator: Allocator, a: u.String, b: u.String) ![]u8 {
    const bytes = try allocator.alloc(u8, a.len + b.len);

    std.mem.copyForwards(u8, bytes, a);
    std.mem.copyForwards(u8, bytes[a.len..], b);

    return bytes;
}

fn useFixedBuffer() !void {
    var buff: [12]u8 = undefined;

    var fba = std.heap.FixedBufferAllocator.init(&buff);
    // defer fba.deinit(); // not exists function.
    // // fixed buffer memory using not os/system managed random space,
    // // but using manual developer array. so, you manage clear buffer only,
    // // not need to manage allocator.

    const allocator = fba.allocator();

    const res = try catAlloc(allocator, he, wd);
    defer allocator.free(res);

    u.print("{s}", .{res});
}

fn sliceOfAlloc(allocator: Allocator, item: anytype, n: usize) ![]@TypeOf(item) {
    const slice = try allocator.alloc(@TypeOf(item), n);
    for (slice) |*e| e.* = item;

    return slice;
}

fn useSliceAlloc() !void {
    const Foo = struct { a: u8 = 42, b: u.String = "HelloWorld" };
    const foo = Foo{};

    var buff: [2 * @sizeOf(Foo)]u8 = undefined;
    // var buff: [2 * hw.len]u8 = undefined;

    var fba = std.heap.FixedBufferAllocator.init(&buff);
    const allocator = fba.allocator();

    const res = try sliceOfAlloc(allocator, foo, 2);
    defer allocator.free(res);

    u.print("{any}", .{res});
}

pub fn run() !void {
    try useFixedBuffer();
    try useSliceAlloc();
}
