const std = @import("std");

pub fn main() !void {
    var buff: [4096]u8 = undefined;
    var bw = std.fs.File.stdout().writer(&buff);
    const stdout = &bw.interface;

    try stdout.print("Hello, world! {}\n", .{std.time.timestamp()});

    try stdout.flush();
}