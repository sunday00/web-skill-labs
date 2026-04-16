const std = @import("std");
const Io = std.Io;
const practice_raylib = @import("practice_raylib");

const rl = @import("raylib");
const global = @import("_common/global.zig");

fn setMainConfigs(alloc: std.mem.Allocator) void {
    global.g.alloc = alloc;
    global.g.screenW = 800;
    global.g.screenH = 600;
}

// pub fn main(init: std.process.Init) !void {
pub fn main() !void {
    // ==========================CONFIGS========================================

    @setEvalBranchQuota(1100); // when reach default maximum inline
    rl.setTraceLogLevel(.none);

    var gpa = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    setMainConfigs(alloc);

    rl.initWindow(global.g.screenW, global.g.screenH, "Zig Raylib Practice");
    defer rl.closeWindow();

    rl.setTargetFPS(60);
    // ==========================CONFIGS========================================

    // try @import("tcp/get.zig").run();
    // try @import("transmission/example.zig").run();
    // try @import("transmission/receive.zig").run();

    try @import("sprite/load.zig").run();
}
