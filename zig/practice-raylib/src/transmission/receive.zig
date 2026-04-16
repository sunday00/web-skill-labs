const std = @import("std");
const rl = @import("raylib");

const global = @import("../_common/global.zig");
var greet: [:0]const u8 = "";
var input_buffer: [1024]u8 = undefined;

export fn getInputBufferPtr() [*]u8 {
    return &input_buffer;
}

export fn processInput(len: usize) void {
    greet = input_buffer[0..len :0];
}

pub fn run() !void {
    greet = "hello test..";

    while (!rl.windowShouldClose()) {
        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(rl.Color.black);

        rl.drawText("Hm...", 40, global.g.screenH - 130, 20, rl.Color.white);
        rl.drawText(greet, 20, global.g.screenH - 100, 20, rl.Color.white);
    }
}
