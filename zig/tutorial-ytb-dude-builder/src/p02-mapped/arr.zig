const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

pub fn run() !void {
    const a1: [5]u8 = .{ 1, 2, 3, 4, 5 };

    u.print("{any}", .{a1});

    const a2 = [_]u8{ 1, 2, 3, 4, 5 };

    u.print("{any}", .{a2});

    const a3 = a2 ** 2; // repeat array

    u.print("{any}", .{a3});

    const ll = 5; // ---------------------------+
    const a4: [ll]u8 = .{0} ** ll; // can define size type   |
    //         ^                                             |
    //         |                                             |
    //         +---------------------------------------------+

    u.print("{any} {d}", .{ a4, a4.len });

    var lazySpecificArr: [2]u8 = undefined;
    lazySpecificArr[0] = 1;
    lazySpecificArr[1] = 2;

    u.print("{any}", .{lazySpecificArr});

    // const m_arr1: [2][2]u8 = [_][_]u8{ .{ 0, 0 }, .{ 1, 1 } }; // <- can't infer inner arr size on definning
    const m_arr1: [2][2]u8 = [_][2]u8{ .{ 0, 0 }, .{ 1, 1 } };
    u.print("{any}", .{m_arr1});

    // // // try sentinel array training
    const sa1: [5:0]u8 = .{ 1, 2, 0, 3, 4 };
    for (sa1) |value| {
        u.print("{d}", .{value});
    }
    u.print("{d} {d}", .{ sa1.len, sa1[5] });

    const a51 = [_]u8{ 1, 2, 3, 4 };
    const a52 = [_]u8{ 11, 12, 13, 14 };

    const a5 = a51 ++ a52;
    u.print("{any}", .{a5});
}
