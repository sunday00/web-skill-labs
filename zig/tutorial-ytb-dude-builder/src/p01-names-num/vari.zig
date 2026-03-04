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

pub fn run() !void {
    u.print("{d}", .{global_const});

    // wrongDefine1(); // not know size run time err
    correctDefin1();
    correctDefin2();

    u.print("{}", .{std.math.inf(f32)});
    u.print("{}", .{std.math.maxInt(u8)});
}
