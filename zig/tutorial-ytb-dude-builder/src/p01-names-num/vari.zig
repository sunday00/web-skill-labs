const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

// general practice - define variables snake_case;
const global_const: u8 = 42;

// function name camelCase;
fn sayHello() void {
    u.ssPrint("hello");
}

fn wrongDefine1() void {
    var ii1 = 1; // mutable variable should define fixed size type.
    ii1 = 2;
    u.print("{d}", .{ii1});
}

fn correctDefin1() void {
    comptime var ii1 = 1; // use keyword "comptime". mutable variable, but size fixed when compile time.
    ii1 = 2;
    u.print("{d}", .{ii1});
}

fn correctDefin2() void {
    var ii1: u8 = 1; // use fixed size type.
    ii1 = 2;
    u.print("{d}", .{ii1});
}

fn gameSwallowHpPotion() u6 {
    return 50;
}

pub fn run() !void {
    u.print("{d}", .{global_const});

    // wrongDefine1(); // not know size run time err
    correctDefin1();
    correctDefin2();

    u.print("{}", .{std.math.inf(f32)});
    u.print("{}", .{std.math.maxInt(u8)});

    const one: u8 = 1;
    const two: u8 = 2;

    var res1 = one + two;
    u.print("{d}", .{res1});

    // res1 = res1 + 550;
    // u.print("{d}", .{res1}); // overflow

    res1 = @as(u8, 255) +% 2; // +% wrapping add. number gose to minimum when reach overflow.
    u.print("{d}", .{res1});

    var gameHp: u7 = 88;
    gameHp = gameHp +| gameSwallowHpPotion(); // +| saturating add. number stop on max when reach overflow.
    u.print("{d}", .{gameHp});

    gameHp = gameHp +| gameSwallowHpPotion();
    u.print("{d}", .{gameHp});

    const nn1: u32 = 1;
    const nn2: u32 = 1;

    const nn3: u16 = nn1 + nn2;
    u.print("{d}", .{nn3});

    const nn4: u8 = @intCast(nn3); // to under size type
    u.print("{d}", .{nn4});

    const ff1: f16 = @floatFromInt(nn4);
    u.print("{d:.2}", .{ff1});

    u.print("{d}", .{@abs(-1)});
    u.print("{d}", .{std.math.sqrt(9)});
    u.print("{d}", .{std.math.cos(9)});
}
