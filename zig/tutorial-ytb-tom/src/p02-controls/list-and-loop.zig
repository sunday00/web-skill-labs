const std = @import("std");

fn print(comptime fmt: []const u8, args: anytype) void {
    std.debug.print(fmt, args);
    std.debug.print("\n", .{});
}

fn ssPrint(args: []const u8) void {
    std.debug.print("{s}\n", .{args});
}

pub fn run() !void {
    const arr1: [5]u8 = .{ 1, 3, 5, 7, 9 };

    for (0..arr1.len) |i| {
        const v = arr1[i];

        print("i: {} | v: {}", .{ i, v });
    }

    ssPrint("=====================");

    for (arr1, 0..) |v, i| { // 0.. means "create row number start from 0. this is not arr index"
        print("i: {} | v: {}", .{ i, v });
    }

    var wi: usize = 0;
    while (wi < 10) {
        ssPrint("hello");

        wi += 1;
    }

    wi = 0;
    while (wi < 10) : (wi += 1) {
        ssPrint("hello22");
    }

    ssPrint("=====================");

    const arr21: [5]u8 = .{ 1, 3, 5, 7, 9 };
    const arr22: [5]u16 = .{ 100, 200, 300, 400, 500 };

    for (arr21, arr22) |v1, v2| {
        print("v1: {d}, v2: {d}", .{ v1, v2 });
    }
}
