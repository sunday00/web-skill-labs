const std = @import("std");
const u = @import("../utils.zig");
const l = std.log;

fn now(allocator: std.mem.Allocator) ![]u8 {
    const kst_offset = 9 * 60 * 60;
    const epoch = std.time.epoch.EpochSeconds{ .secs = @intCast(std.time.timestamp() + kst_offset) };

    const day_seconds = epoch.getDaySeconds();

    return std.fmt.allocPrint(allocator, "{d:0>2}:{d:0>2}:{d:0>2}", .{
        day_seconds.getHoursIntoDay(),
        day_seconds.getMinutesIntoHour(),
        day_seconds.getSecondsIntoMinute(),
    });
}

pub fn run() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var chk: std.heap.Check = undefined;
    defer chk = gpa.deinit();

    const ALLOCATOR = gpa.allocator();

    const dt = try now(ALLOCATOR);
    defer ALLOCATOR.free(dt);

    l.info("{s}: {s}\n", .{ dt, "this is different with debug print." });

    l.err("{s}", .{"oooooppppssss"});

    u.print("{any}", .{chk});
}
