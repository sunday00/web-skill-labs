const std = @import("std");
const rl = @import("raylib");

const global = @import("../_common/global.zig");

pub fn run() !void {
    var gpa = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    // rl.setTraceLogLevel(rl.TraceLogLevel.all);

    var codepoints: std.ArrayList(i32) = .empty;

    for (32..127) |i| try codepoints.append(alloc, @intCast(i));
    var cp: i32 = 0xAC00;
    while (cp <= 0xD7A3) : (cp += 1) {
        try codepoints.append(alloc, cp);
    }
    defer codepoints.deinit(alloc);

    const font = try rl.loadFontEx(
        "src/assets/neodgm_code.ttf",
        15,
        codepoints.items,
    );

    // rl.setTextureFilter(font.texture, .bilinear);

    while (!rl.windowShouldClose()) {
        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(rl.Color.black);

        rl.drawTextEx(
            font,
            "누르시오",
            .{ .x = 20, .y = global.g.screenH - 130 },
            20,
            3,
            rl.Color.white,
        );
    }
}
