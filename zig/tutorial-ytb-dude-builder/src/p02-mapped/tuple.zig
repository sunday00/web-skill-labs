const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");
const lib = @import("../lib.zig");

fn stand() !void {
    const t1: struct { u8, bool } = .{ 1, true };
    u.print("{} {}", t1);
    u.print("{} {}", .{ t1[0], t1[1] });
    u.print("{} {}", .{ t1.@"0", t1.@"1" });
    u.print("{}", .{t1.len});

    const p1 = &t1; // accessing each value via pointer is same
    u.print("{} {}", .{ p1[0], p1[1] });
    u.print("{} {}", .{ p1.@"0", p1.@"1" });
    u.print("{}", .{p1.len});
}

fn cat() !void {
    const t1: struct { bool, bool } = .{ true, true };
    const t2: struct { u8, u8 } = .{ 3, 4 };

    u.print("{any} {}", .{ t1 ++ t2, @TypeOf(t1 ++ t2) });

    const arr1: [3]bool = .{ true, false, true };
    const arr2 = arr1 ++ t1; // <-- mostly updated to typed arr;
    u.print("{any} {}", .{ arr2, @TypeOf(arr2) });
}

fn rep() !void {
    const t1: struct { bool, bool } = .{ true, true };

    u.print("{any}", .{t1 ** 2});
}

fn kv() !void {
    const t1: struct { bool, u8, f16, u.String } = .{ true, 1, 1.3, "hello" };
    // for (t1, 0..) |value, i| { // <-- normal for doesn't work.
    inline for (t1, 0..) |value, i| { // <-- inline keyword mandatory.
        u.print("{}, {any}", .{ i, value });
    }
}

pub fn main() !void {
    try stand();
    try cat();
    try rep();
    try kv();
}
