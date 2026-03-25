const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

pub fn run() !void {
    var arr = [_]u8{ 0, 1, 2, 3, 4 };
    const arrClone = arr[0..arr.len];

    u.print("{any} {any} {}", .{ arr, arrClone, @TypeOf(arrClone) });

    const ref_arr = &arr;
    ref_arr[0] += 1; // const, but a[0] goes to change. because this is just pointing var array.

    u.print("{any} {any} {} {any}", .{ arr, ref_arr, @TypeOf(ref_arr), ref_arr.ptr });

    // const arr2 = [_]u8{ 0, 1, 2, 3 };
    // arr2[0] += 1; // this cant be update.

    // u.print("{any}", .{arr2});

    // const ref_arr2: []const u8 = &arr;
    // ref_arr2[0] += 1; // this cant be update too. because defining const [].

    var ss1 = arr[0..];
    u.print("{any}", .{ss1});

    ss1.ptr += 2;
    u.print("{any}", .{ss1});

    ss1.ptr -= 2;
    u.print("{any}", .{ss1});

    ss1.ptr -= 2;
    u.print("{any}", .{ss1});

    const ss2 = arr[1..3];
    u.print("{any}", .{ss2});

    var arrS = [_][:0]const u8{ "a", "b", "c" };
    u.print("{any}", .{arrS});

    var ss3 = arrS[0..];
    u.print("{any}", .{ss3});

    ss3.ptr += 1;
    u.print("{any}", .{ss3});

    ss3.ptr += 1;
    u.print("{any}", .{ss3});

    ss3.ptr += 1;
    u.print("{any}", .{ss3});

    // ss3.ptr += 1;
    // u.print("{any}", .{ss3});

    // ss3.ptr += 1;
    // u.print("{any}", .{ss3}); // somethings happen and err like stack overflow

    var arr3 = [_]u8{ 1, 2, 3, 4, 0 };
    const e_s: [:0]u8 = arr3[0..4 :0];
    u.print("{any}", .{e_s});

    const arr4 = [_]u8{ 1, 2, 3, 4, 5, 6, 7, 8 };
    const f_slice = arr4[2..][0..4]; // slicing chain
    u.print("{any}", .{f_slice});
}
