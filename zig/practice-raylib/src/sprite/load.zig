const std = @import("std");
const rl = @import("raylib");

const global = @import("../_common/global.zig");

pub fn run() !void {
    const texture = rl.loadTexture("src/assets/knight.png") catch |err| {
        std.debug.print("{any}\n", .{err});
        return;
    };

    defer rl.unloadTexture(texture);

    // 원본 이미지의 어느 영역을 가져올 것인가 (전체)
    const sourceRec = rl.Rectangle{
        .x = 0,
        .y = 0,
        .width = @as(f32, @floatFromInt(texture.width)),
        .height = @as(f32, @floatFromInt(texture.height)),
    };

    // 화면 어디에, 어떤 크기로 그릴 것인가 (원하는 크기 지정)
    const destRec = rl.Rectangle{
        .x = 20,
        .y = 20,
        .width = 40, // 가로 400픽셀로 고정
        .height = 40, // 세로 300픽셀로 고정
    };

    while (!rl.windowShouldClose()) {
        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(rl.Color.black);

        // 중심점 (회전 기준점)
        const origin = rl.Vector2{ .x = 0, .y = 0 };

        rl.drawTexturePro(texture, sourceRec, destRec, origin, 0.0, rl.Color.white);
    }
}
