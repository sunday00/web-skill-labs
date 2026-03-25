const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

pub fn run() !void {
    const arr1 = [_]u8{ 0, 1, 2, 3, 4, 5 };

    for (arr1) |value| {
        u.print("{}", .{value});
    }

    for (arr1, 0..) |value, i| {
        u.print("{}, {}", .{ i, value });
    }

    var arr2 = [_]u8{ 10, 20, 30, 40, 50, 60 };
    var arr3: [arr1.len]u8 = undefined;
    for (0..arr1.len, arr1[0..], arr2[0..]) |i, value1, value2| {
        arr3[i] = value1 + value2;
    }
    u.print("{any}", .{arr3});

    var vvv: u8 = undefined;
    const r = u.rand(0, 10);
    for (0..10) |value| {
        if (value == r) {
            vvv = @as(u8, r);
            break;
        }
    }
    u.print("{d} {d}", .{ vvv, r });

    for (&arr2) |*item| {
        item.* *= 2;

        u.print("{}", .{item.*});
    }
    u.print("{any}", .{arr2});

    const oArr = [_]?u8{ 0, 1, 2, null, null };
    const filtered = for (oArr, 0..) |n, i| {
        if (n == null) break oArr[0..i];
} else oArr[0..];
    u.print("{any}", .{filtered});
}
