const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

pub fn run() !void {
    u.nsPrint(.{ 1, 2, 3, 4, 5 });

    const x: u8 = blk: {
        const innerV1 = 1;
        const innerV2 = 2;

        break :blk innerV1 + innerV2;
    };

    u.nsPrint(.{x});

    switch (x) {
        0...10 => u.ssPrint("small"),

        blk: {
            const innerV1 = 11;
            const innerV2 = 12;

            break :blk innerV1 + innerV2;
        } => u.ssPrint("over 10 comptime know value. In this example, x == 23 -> do something..."),

        else => u.nsPrint("big"),
    }

    const ans: u8 = switch (x) {
        0...5 => 1,
        else => 0,
    };

    u.nsPrint(.{ans});
}
