const std = @import("std");

pub fn print(comptime fmt: []const u8, args: anytype) void {
    std.debug.print(fmt, args);
    std.debug.print("\n", .{});
}

pub fn ssPrint(args: []const u8) void {
    std.debug.print("{s}\n", .{args});
}
