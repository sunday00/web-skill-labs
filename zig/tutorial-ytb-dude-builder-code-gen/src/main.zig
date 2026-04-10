const std = @import("std");

const fibs = @import("fibs.zig").fibs;

pub fn main() !void {
    for (&fibs, 1..) |n, i| std.debug.print("fib {}: {}\n", .{ i, n });
}