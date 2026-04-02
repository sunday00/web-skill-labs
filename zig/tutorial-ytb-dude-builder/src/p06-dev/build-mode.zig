const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

const Lib = @import("../lib.zig");
const List = Lib.LIST.List;

pub fn run() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .verbose_log = builtin.mode == .Debug }).init;
    defer u.print("{}", .{gpa.deinit()});

    const allocator = gpa.allocator();
    var list = try List(u8).init(allocator, 42);
    defer list.deinit();

    try list.append(13);
    try list.append(99);

    std.log.debug("{}", .{list.lookup(13)});
    std.log.info("{}", .{list.lookup(13)});
}
