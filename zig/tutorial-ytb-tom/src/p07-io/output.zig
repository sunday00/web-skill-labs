const std = @import("std");
const builtin = @import("builtin");
const fsFile = std.fs.File;
const u = @import("../utils.zig");

pub fn run() !void {
    var buffer: [1024]u8 = undefined;
    var fsWriter = std.fs.File.stdout().writer(&buffer);
    const writer: *std.Io.Writer = &fsWriter.interface;

    try writer.print("{s}\n", .{"hello"});
    try writer.flush();
}
