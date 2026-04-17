const std = @import("std");
const rl = @import("raylib");

const global = @import("../_common/global.zig");

pub fn run() !void {
    // rl.setTraceLogLevel(rl.TraceLogLevel.all);

    const sound_data align(8) = @embedFile("../assets/jump.mp3");
    if (sound_data.len == 0) {
        @panic("Sound data is empty!");
    }

    rl.initAudioDevice();
    defer rl.closeAudioDevice();

    const wav = try rl.loadWaveFromMemory(".mp3", sound_data);
    if (@as(?*anyopaque, wav.data) == null) {
        std.debug.print("Failed to load Wave from memory. Check codec support!\n", .{});
        return;
    }
    defer rl.unloadWave(wav);

    const soundIns = rl.loadSoundFromWave(wav);
    defer rl.unloadSound(soundIns);

    while (!rl.windowShouldClose()) {
        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(rl.Color.black);

        if (rl.isKeyPressed(rl.KeyboardKey.space)) {
            rl.playSound(soundIns);
        }

        rl.drawText("누르시오", 40, global.g.screenH - 130, 20, rl.Color.white);
    }
}
