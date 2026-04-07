const std = @import("std");
const conf = @import("my_conf");

pub fn main() !void {
    std.debug.print("{s}", .{conf.ver});
}
