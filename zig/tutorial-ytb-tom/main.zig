const std = @import("std");
const print = std.debug.print;

fn printString(comptime input: []const u8) void {
    std.debug.print(input, .{});
}

fn add(a: i32, b: i32) i32 {
    return a + b;
}

pub fn main() void {
    // std.debug.print("hello {} world! {} and {}!", .{ 10, 20, 100 });
    // std.debug.print("hello {} world! {} and {}!", .{ 10, 20 });

    printString("hello world");

    std.debug.print("{}", .{add(1, 2)});

    // ----------------------------------------------
    // const, variables, define, re-define, type, ignore

    _ = 1 + 1; // not using value

    const res1 = 11;
    var res2: []const u8 = undefined;

    std.debug.print("{}\n", .{res1});
    // std.debug.print("{}", .{res2});

    res2 = "updated";
    std.debug.print("{s}\n", .{res2});

    const ii1: i8 = 8;
    std.debug.print("{d}\n", .{ii1});

    const ii2: i7 = 8;
    print("{d}\n", .{ii2});

    // ----------------------------------------------
    // string

    const ss1: []const u8 = "hello my string example";
    print("{s}\n", .{ss1});

    const len: usize = ss1.len;
    print("{}\n", .{len});

    // ----------------------------------------------
    // slice list
    const arr1 = [_]i32{ 1, 2, 3, 4, 5 };
    const arr2: [5]i32 = .{ 1, 2, 3, 4, 5 };
    print("{any} | {any}\n", .{ arr1, arr2 });

    const sli1: []const i32 = arr1[0..];
    print("{any}\n", .{sli1});

    var arr3: [5]i32 = .{ 1, 2, 3, 4, 5 };
    const sli2: []i32 = arr3[0..2]; // slice ref shallow copy. not deep copy.
    sli2[1] = 100;
    print("{any} | {any}\n", .{ arr3, sli2 });
}
