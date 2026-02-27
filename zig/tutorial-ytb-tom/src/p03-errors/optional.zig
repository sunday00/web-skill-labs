const std = @import("std");
const u = @import("../utils.zig");

fn optionalReturn(a: u8) ?u8 {
    if (a % 2 == 0) {
        return 1;
    }

    return null;
}

pub fn run() !void {
    const el1: ?u8 = optionalReturn(1);

    if (el1 == null) {
        u.ssPrint("a is not even");
    } else {
        u.print("{}\n", .{el1.?}); // .? means definitely not null (like ! in JS).
    }
}
