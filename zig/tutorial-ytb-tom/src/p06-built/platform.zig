const std = @import("std");
const B = @import("builtin");
const u = @import("../utils.zig");

pub fn run() !void {
    u.print("{any}", .{B.os.tag}); // .macos

}
