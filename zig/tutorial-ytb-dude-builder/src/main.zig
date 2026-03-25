const std = @import("std");
const baseVari = @import("p01-names-num/vari.zig");
const mapped = @import("p02-mapped/root.zig");
const bo = @import("p03-control/bool.zig");
const sw = @import("p03-control/switch.zig");
const pnt = @import("p04-core/pointer.zig");
const sli = @import("p02-mapped/slice.zig");

pub fn main() !void {
    // try baseVari.run();
    // try mapped.arr.run();
    // try bo.run();
    // try sw.run();
    // try mapped.en.run();
    // try pnt.run();
    try sli.run();
}
