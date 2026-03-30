const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

const InputError = error{
    Empty,
};

const NumberError = error{
    InvalidCharacter,
    Overflow,
    DivideZero,
    ReachedZero,
};

const ParseError = InputError || NumberError;

fn parseNum(s: []const u8) ParseError!u8 { // throwable unwrap u8
    if (s.len == 0) return error.Empty;

    return std.fmt.parseInt(u8, s, 10);
}

var c: usize = undefined;
fn countDown() anyerror!usize {
    return if (c == 0) error.ReachedZero else blk: {
        c -= 1;
        break :blk c;
    };
}

pub fn run() !void {
    const r1 = parseNum("");
    u.print("{any}", .{r1});
    u.print("{!}", .{r1});

    const r2 = parseNum("12");
    u.print("{any}", .{r2});
    u.print("{!}", .{r2});

    const r3 = parseNum("") catch 100;
    u.print("{}", .{r3});

    const r4 = parseNum("") catch |e| switch (e) {
        error.Empty => blk: {
            u.ssPrint("err empty");
            break :blk 77;
        },
        else => |ee| return ee,
    };

    u.dsPrint(r4);

    // const r5 = parseNum("") catch unreachable;
    const r5 = parseNum("123") catch unreachable;
    // developer definitely already know "NO ERROR".
    // so, skip unwrapping the throw or catch block, just use returning value, focus on main logic.

    u.dsPrint(r5);

    // const r6 = parseNum("") catch |e| return e;
    // u.print("{any}", .{r6});
    //
    // const r7 = try parseNum("");
    // u.print("{any}", .{r7});
    //
    // // r6 short cut === r7.
    // // break all below logic, returning out this local func.

    if (parseNum("")) |num| {
        u.dsPrint(num);
    } else |e| u.print("{any}", .{e}); // if/else works like try catch.

    c = 3;
    while (countDown()) |n| {
        u.print("{any}", .{n});
    } else |e| u.print("{any}", .{e});
}
