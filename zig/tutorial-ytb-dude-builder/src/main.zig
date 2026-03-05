const std = @import("std");
const baseVari = @import("p01-names-num/vari.zig");
const arr = @import("p02-mapped/arr.zig");
const bo = @import("p03-control/bool.zig");
const sw = @import("p03-control/switch.zig");

pub fn main() !void {
    // try baseVari.run();
    // try arr.run();
    // try bo.run();
    try sw.run();
}
