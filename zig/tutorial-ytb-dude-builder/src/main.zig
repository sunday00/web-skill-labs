const std = @import("std");
const builtin = @import("builtin");
const root = @import("root");
const baseVari = @import("p01-names-num/vari.zig");
const mapped = @import("p02-mapped/root.zig");
const bo = @import("p03-control/bool.zig");
const sw = @import("p03-control/switch.zig");
const pnt = @import("p04-core/pointer.zig");
const sli = @import("p02-mapped/slice.zig");
const fl = @import("p03-control/loop-for.zig");
const wl = @import("p03-control/loop-while.zig");
const ff = @import("p04-core/func.zig");
const err = @import("p06-dev/err.zig");
const str = @import("p01-names-num/strings.zig");
const stct = @import("p02-mapped/oop.zig");
const co = @import("p04-core/comp.zig");
const any = @import("p04-core/any.zig");
const tt = @import("p06-dev/testing.zig");
const mb = @import("p07-memory/basic.zig");
const mo = @import("p07-memory/obj.zig");
const fb = @import("p07-memory/fixed-buffer.zig");
const areb = @import("p07-memory/arena.zig");
const etcM = @import("p07-memory/etc.zig");
const mp = @import("p07-memory/pool.zig");
const buildMode = @import("p06-dev/build-mode.zig");
const formatString = @import("p01-names-num/format.zig");

pub const std_options: std.Options = .{
    .log_level = if (builtin.mode == .Debug) .debug else .info,
};

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
    // try co.run();
    // try any.run();
    // try tt.run();
    // try mb.run();
    // try mo.run();
    // try fb.run();
    // try areb.run();
    // try etcM.run();
    // try mp.run();
    // try buildMode.run();
    // try formatString.main();
    // try @import("p02-mapped/cast.zig").run();

    // try @import("p01-names-num/dynamic.zig").run();
    // std.debug.print("\n\n=======\n\n", .{});
    // try @import("p02-mapped/tuple.zig").main();

    // try @import("p07-memory/mem-layout.zig").main();

    // try @import("p02-mapped/arr-list.zig").main();

    // try @import("p02-mapped/hashmap.zig").main();

    // try @import("p04-core/withc.zig").main();

    // try @import("p04-core/with-3.zig").main();

    // try @import("p06-dev/build-opt.zig").main();
    // try @import("p06-dev/running-opt.zig").main();

    // try @import("p05-interface/main.zig").main();

    try @import("p02-mapped/vertor.zig").main();
}

test {
    @import("std").testing.refAllDecls(@This());
}
