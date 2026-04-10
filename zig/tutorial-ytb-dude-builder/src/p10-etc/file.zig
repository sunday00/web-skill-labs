const std = @import("std");
const builtin = @import("builtin");

const fs = std.fs;
const fmt = std.fmt;

fn populate(dir: *fs.Dir, force: ?usize) !void {
    const fileCnt = force orelse @as(usize, @intCast(dir.fd));

    std.debug.print("{}\n", .{fileCnt});

    for (0..fileCnt) |i| {
        var buf: [8]u8 = undefined;
        const filename = try fmt.bufPrint(&buf, "file_{}", .{i});

        var file = try dir.createFile(filename, .{});
        defer file.close();

        var bw = file.writer(&buf);
        const writer = &bw.interface;

        _ = try writer.print("This is file_{}", .{i});
        try writer.flush();
    }
}

pub fn main() !void {
    var generatedSub2 = try fs.cwd().makeOpenPath("test_dir/sub_1/sub_2", .{}); // create folder recursive

    try populate(&generatedSub2, null);

    // ================== delete on program exit ====================
    defer fs.cwd().deleteTree("test_dir") catch |err| {
        std.debug.print("err: {}\n", .{err});
    };
    defer generatedSub2.close();
    // ================== delete on program exit ====================

    try populate(&generatedSub2, 3);

    var generatedSub1 = try fs.cwd().openDir("test_dir/sub_1", .{});
    defer generatedSub1.close();
    try populate(&generatedSub1, 3);

    var td = try fs.cwd().openDir("test_dir", .{});
    defer td.close();
    try populate(&td, 3);

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var iterDir = try fs.cwd().openDir("test_dir", .{ .iterate = true });
    defer iterDir.close();

    var walker = try iterDir.walk(allocator);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        std.debug.print("{s} | {s}\n", .{ entry.path, @tagName(entry.kind) });

        if (entry.kind == .file) {
            var file = try entry.dir.openFile(entry.basename, .{});
            defer file.close();

            var buf: [1024]u8 = undefined;
            var br = file.reader(&buf);

            const reader = &br.interface;

            outer: while (reader.takeDelimiterExclusive('\n')) |line| {
                std.debug.print("{s}\n", .{line});
            } else |err| switch (err) {
                error.EndOfStream => break :outer, // is really best??
                else => std.debug.print("{}", .{err}),
            }
        }

        std.debug.print("\n", .{});
    }
}
