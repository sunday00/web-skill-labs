const std = @import("std");
const rl = @import("raylib");

const global = @import("../_common/global.zig");

extern fn sendDataToJS(ptr: [*]const u8, len: usize) void;

pub fn doSomethingAndNotify() void {
    const message = "Hello, JavaScript! This is Zig speaking....";

    // 데이터를 준비하고 JS 함수를 호출하여 "이벤트"를 알림
    sendDataToJS(message.ptr, message.len);
}

pub fn run() !void {
    var threaded: std.Io.Threaded = .init_single_threaded;
    const io = threaded.io();

    try io.sleep(.fromSeconds(3), std.Io.Clock.awake);

    doSomethingAndNotify();

    while (!rl.windowShouldClose()) {
        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(rl.Color.black);

        rl.drawText("hello test", 20, global.g.screenH - 100, 20, rl.Color.white);
    }
}
