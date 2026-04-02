const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

const List = @import("list");

pub fn run() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true }).init;
    defer u.print("{}", .{gpa.deinit()});

    const allocator = gpa.allocator();
    var list = try List(u8).init(allocator, 42);
    defer list.deinit();

    try list.append(13);
    try list.append(99);

    u.print("{}", .{list.lookup(13)});
}
