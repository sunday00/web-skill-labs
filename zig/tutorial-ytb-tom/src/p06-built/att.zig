const std = @import("std");
const u = @import("../utils.zig");

fn printInfo(v: anytype) void {
    const T = @TypeOf(v);
    const info = @typeInfo(T);
    const t = @Type(info);

    // comptime {
    //     if (info != .comptime_int) {
    //         const k = v;
    //         u.print("{}", .{k});
    //     }
    // }

    u.print("{}, {}, {}, {}", .{ T, info, t, T == t });
}

pub fn run() !void {
    printInfo("how anout this?");
    printInfo(45);

    const ii: u8 = 1;
    const ii2 = @as(u16, ii);

    u.print("{}, {}, {}", .{ ii, ii2, ii == ii2 });

    const arr1 = [_]u8{ 1, 2, 3, 4, 5 };
    var arr2: [5]u8 = undefined;

    @memcpy(&arr2, &arr1);

    arr2[2] = 7;

    u.print("{any} : {any}", .{ arr1, arr2 });

    const fv = 123.456;
    const iv: u16 = @intFromFloat(fv);

    u.print("{}", .{iv});

    const ee = enum { A, B, C };
    const ve = ee.B;

    u.print("{}", .{@intFromEnum(ve)});

    // const vei: ee = @enumFromInt(2);
    // u.print("{}", .{vei});
    u.print("{}", .{@as(ee, @enumFromInt(2))});

    const s = @src();
    u.print("{s} @ fn {s} () - {d}:{d}", .{ s.file, s.fn_name, s.line, s.column });
}
