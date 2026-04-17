const std = @import("std");
const rl = @import("raylib");

const global = @import("../_common/global.zig");

pub fn run() !void {
    var gpa = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    // rl.setTraceLogLevel(rl.TraceLogLevel.all);

    const font_data align(8) = @embedFile("../assets/neodgm_code.ttf");
    // const font_data align(8) = @embedFile("../assets/silkscreen.ttf");
    var codepoints: std.ArrayList(i32) = .empty;
    // var codepoints = try std.ArrayList(i32).initCapacity(
    //     alloc,
    //     0,
    // );

    for (32..127) |i| try codepoints.append(alloc, @intCast(i));
    // for (32..33) |i| try codepoints.append(global.g.alloc, @intCast(i));
    var cp: i32 = 0xAC00;
    while (cp <= 0xD7A3) : (cp += 1) {
        // while (cp <= 0xAD00) : (cp += 1) {
        try codepoints.append(alloc, cp);
    }
    // try codepoints.append(global.g.alloc, 0xAD00);

    // try codepoints.ensureTotalCapacity(alloc, 1);

    // codepoints.appendAssumeCapacity('누');
    // codepoints.appendAssumeCapacity('르');
    // codepoints.appendAssumeCapacity('시');
    // codepoints.appendAssumeCapacity('오');
    defer codepoints.deinit(alloc);

    const font = try rl.loadFontFromMemory(
        ".ttf",
        font_data,
        11,
        codepoints.items,
    );

    // const font = try rl.loadFontFromMemory(
    //     ".ttf",
    //     font_data,
    //     15,
    //     null,
    // );
    // defer rl.unloadFont(font);

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
