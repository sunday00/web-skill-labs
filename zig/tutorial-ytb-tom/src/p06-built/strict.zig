const std = @import("std");
const u = @import("../utils.zig");

pub fn run() !void {
    // var tn: u8 = 255;
    comptime var tn: u8 = 255;

    // tn += 1;
    // u.print("{d}", .{tn});

    tn +%= 1; // forced to add and start rotate number from 0.
    //        // ignore integer overflow.
    u.print("{d}", .{tn});

    tn +%= 1;
    u.print("{d}", .{tn});

    // const bn: u16 = 256;
    // const sn: u8 = bn;
    // u.print("{d}", .{sn});

    const bn: u16 = 256;
    const sn: u8 = @truncate(bn);
    u.print("{d}", .{sn});
}
