const std = @import("std");
const print = std.debug.print;

pub fn run() !void {
    // -------------- conditions

    if (1 == 1) {
        print("sleepy", .{});
    }
}
