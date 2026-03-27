const std = @import("std");
const baseVari = @import("p01-names-num/vari.zig");
const mapped = @import("p02-mapped/root.zig");
const bo = @import("p03-control/bool.zig");
const sw = @import("p03-control/switch.zig");
const pnt = @import("p04-core/pointer.zig");
const sli = @import("p02-mapped/slice.zig");
const fl = @import("p03-control/loop-for.zig");
const wl = @import("p03-control/loop-while.zig");
const ff = @import("p04-core/func.zig");
const err = @import("p04-core/err.zig");
const str = @import("p01-names-num/strings.zig");
const stct = @import("p02-mapped/oop.zig");
const co = @import("p04-core/comp.zig");

pub fn main() !void {
    // try baseVari.run();
    // try mapped.arr.run();
    // try bo.run();
    // try sw.run();
    // try mapped.en.run();
    // try pnt.run();
    // try sli.run();
    // try fl.run();
    // try wl.run();
    // try ff.run();
    // try err.run();
    // try str.run();
    // try stct.run();
    try co.run();
}
