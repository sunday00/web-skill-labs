const std = @import("std");

pub fn print(comptime fmt: []const u8, args: anytype) void {
    std.debug.print(fmt, args);
    std.debug.print("\n", .{});
}

pub fn ssPrint(args: []const u8) void {
    std.debug.print("{s}\n", .{args});
}

pub fn dsPrint(arg: u64) void {
    std.debug.print("{}\n", .{arg});
}

pub fn nsPrint(args: anytype) void {
    const fmt = "{}   " ** args.len;
    std.debug.print(fmt, args);
    std.debug.print("\n", .{});
}

pub fn rand(min: u8, max: u8) u8 {
    const rd = std.crypto.random;

    return rd.intRangeAtMost(u8, min, max);
}
