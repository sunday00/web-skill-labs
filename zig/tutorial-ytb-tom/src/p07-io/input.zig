const std = @import("std");
const builtin = @import("builtin");
const fsFile = std.fs.File;
const u = @import("../utils.zig");

pub fn run() !void {
    const allocator = std.heap.page_allocator;
    const args: [][:0]u8 = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    for (args, 0..) |value, i| {
        u.print("{s}, {d}", .{ value, i });
    }

    u.ssPrint("type close to exit");

    const file_path = "log.txt";
    var file = try std.fs.cwd().createFile(file_path, .{
        .truncate = false,
    });

    defer file.close();

    var ui: [60]u8 = undefined;
    var fsReader: fsFile.Reader = fsFile.stdin().reader(&ui);
    const reader: *std.Io.Reader = &fsReader.interface;

    // const line = try reader.takeDelimiterExclusive('\n');
    const deletemiter = if (builtin.target.os.tag == .windows) '\r' else '\n';

    while (true) {
        const line = try reader.takeDelimiterExclusive(deletemiter);

        // var next_chr: []u8 = try reader.peek(1);
        // if (next_chr[0] == '\r') reader.toss(1);
        //
        // next_chr = try reader.peek(1);
        // if (next_chr[0] == '\n') reader.toss(1);

        if (std.mem.eql(u8, line, "close") == true) break;

        try file.seekFromEnd(0);
        try file.writeAll(line);
        try file.writeAll("\n");
    }
}
