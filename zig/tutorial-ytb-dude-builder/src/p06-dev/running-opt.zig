const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");
const conf = @import("my_conf");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    const toolMap = std.StringHashMap(u.String);
    var tool = toolMap.init(alloc);
    defer tool.deinit();

    for (args, 0..) |arg, idx| {
        std.debug.print("{} {s}\n", .{ idx, arg });

        if (idx == 0) continue;

        var kv = std.mem.splitScalar(u8, arg, '=');
        const k = kv.next() orelse "";
        const v = kv.next() orelse "";

        try tool.put(k, v);
    }

    std.debug.print("{s} {s}", .{ tool.get("title").?, tool.get("price").? });
}
