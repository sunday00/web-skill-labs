const std = @import("std");

pub fn main() !void {
    std.debug.print("run this first\n", .{});

    var args = std.process.args();
    _ = args.next(); // Skip running binary name

    // Access filename arg passed in from build.zig.
    const filename = args.next().?;

    // By using the passed-in arg as the filename, this
    // file is created in the build system's cache and
    // will only be re-generated if necessary.
    std.debug.print("{s}\n", .{filename});
    var file = try std.fs.cwd().createFile(filename, .{  });
    defer file.close();
    // var bw = std.io.bufferedWriter(file.writer());
    var buffer: [4096]u8 = undefined;
    var bw = file.writer(&buffer);
    const writer = &bw.interface;

    // const writer = bw.writer();

    const content_head =
        \\pub const fibs = [_]usize{
    ;
    writer.writeAll(content_head) catch |err| {
        std.debug.print("{}", .{err});
    };

    // Access fib-end arg passed in from build.zig.
    const n = try std.fmt.parseInt(usize, args.next().?, 10);

    for (0..n) |i| {
        if (i != 0) try writer.writeAll(", ");
        try writer.print("{}", .{fib(i)});
    }

    const content_tail =
        \\ };
        \\
    ;
    try writer.writeAll(content_tail);

    try writer.flush();
}

fn fib(n: usize) usize {
    if (n < 2) return n;

    var a: usize = 0;
    var b: usize = 1;

    for (0..n) |_| {
        const tmp = a;
        a = b;
        b = tmp + b;
    }

    return a;
}
