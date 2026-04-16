const std = @import("std");
const rl = @import("raylib");

const global = @import("../_common/global.zig");

extern fn callJs(ptr: [*]const u8) void;

pub fn run() !void {
    var threaded: std.Io.Threaded = .init_single_threaded;
    const io = threaded.io();

    try io.sleep(.fromSeconds(3), std.Io.Clock.awake);

    callJs("logKeyValue");

    while (!rl.windowShouldClose()) {
        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(rl.Color.black);

        rl.drawText("hello test .. ", 20, global.g.screenH - 100, 20, rl.Color.white);
    }
}
