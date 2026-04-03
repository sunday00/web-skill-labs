const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");
const lib = @import("../lib.zig");

fn add(a: u8, b: u16) u32 {
    return a + b;
}

fn intSizeCast() !void {
    const a: u8 = 42;
    const b: u16 = a;
    const c: u32 = b;
    const d: u64 = c;
    const e: u128 = d;

    u.print("{d} {}", .{ e, @TypeOf(e) });

    const f: u4 = @truncate(e); // forced dump away over max value
    u.print("{d} {}", .{ f, @TypeOf(f) });

    u.print("u8 + u16 -> u32 : {}", .{@TypeOf(add(a, b))});
}

fn floatCast() !void {
    const a: f16 = 3.1415;
    const b: f32 = a;
    const c: f64 = b;
    const d: f128 = c;

    u.print("{d} {}", .{ d, @TypeOf(d) });
    u.print("{}", .{std.math.approxEqAbs(f128, a, d, 0.001)}); // see float compare
}

fn intFloat() !void {
    const a: u8 = 42;
    const b: u16 = 255;

    u.print("{} {}", .{ a + b, @TypeOf(a + b) });

    // const a2: u8 = 200;
    // const b2: u8 = 200;
    //
    // u.print("{} {}", .{ a2 + b2, @TypeOf(a2 + b2) });  // overflow

    const ff1: f16 = 1;
    const ii1: u8 = 1;
    u.print("{} {}", .{ ff1 + ii1, @TypeOf(ff1 + ii1) });

    const ff2: f16 = 1;
    // const ii2: u8 = ff2; // <-- this is error. not like JS, not dump fractional small number. // // ex) (NOT) 3.12 -> 3
    const ii2: u8 = @intFromFloat(ff2);
    const ff3: f16 = @floatFromInt(ii2);
    u.print("{} {} {} {}", .{ ii2, ff3, @TypeOf(ii2), @TypeOf(ff3) });
}

fn arr() !void {
    const a = [_]u8{ 1, 2, 3 };

    const b: []const u8 = &a;
    const b2: []const u8 = a[0..];

    const c: [*]const u8 = &a; // hm... many item pointer type.

    u.print("{*} {*} {*} {*}", .{ &a, b, b2, c });
    u.print("{} {} {} {}", .{ @TypeOf(a), @TypeOf(b), @TypeOf(b2), @TypeOf(c) });
    u.print("{} {} {} {}", .{ a.len, b.len, b2.len, @TypeOf(c[0]) });
}

fn optional() !void {
    var a: ?u8 = null;
    a = 42;

    var b: anyerror!u8 = error.InvalidNumber;
    b = 42;

    const m = if (0 > 12) a else 3.1415;
    u.print("{} {}", .{ m, @TypeOf(m) });
}

fn pointCast() !void {
    const a = [_]u8{ 1, 2, 3 };
    const b = a[0..];
    const c: [*]const u8 = @ptrCast(b);

    u.print("{*} {*} {*} {}", .{ &a, b, c, @TypeOf(c) });
}

pub fn run() !void {
    try intSizeCast();
    try floatCast();
    try intFloat();

    try arr();
    try optional();

    try pointCast();
}
