const std = @import("std");
const print = std.debug.print;

const JobType = enum {
    Doctor,
    Lawyer,
    Teacher,
};

const User = struct {
    name: []const u8,
    age: usize = 0,
    occupation: JobType = .Doctor,

    // pub fn format(
    //     // self: User,
    //     self: @This(),
    //     comptime fmt: []const u8,
    //     options: std.fmt.FormatOptions,
    //     writer: anytype,
    // ) !void {
    //     // fmt나 options를 사용하지 않더라도 선언은 되어 있어야 함
    //     _ = fmt;
    //     _ = options;
    //
    //     // "User: 이름(나이)" 형태로 출력 시도
    //     try writer.print("Custom Format -> User: {s} ({d}, {s})", .{
    //         self.name,
    //         self.age,
    //         @tagName(self.occupation),
    //     });
    // }
    //

    pub fn toString(self: @This()) void {
        print("User: {s} ({d}, {s})", .{
            self.name,
            self.age,
            @tagName(self.occupation),
        });
    }
};

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

    // -- pointer --

    const ptr1: *i32 = &arr3[0];
    ptr1.* = 0;
    print("{}, {any}\n", .{ ptr1, arr3 });

    // const
    var exNo: i32 = 10;
    const ptr2: *i32 = &exNo;
    ptr2.* = 11;

    print("{}\n", .{exNo});

    const exNo22: i32 = 10;
    const ptr22: *const i32 = &exNo22;
    // ptr22.* = 11;

    print("{}\n", .{ptr22.*});

    // ----------------------------------------------
    // chars

    // const sli21: []const u8 = "Hello English";
    const letter21: u8 = 'A';
    const letter22: u8 = 65;

    print("{}, {}, {c}\n", .{ letter21, letter22, letter22 });

    const ss21: []const u8 = "hello my string example";
    const ss22: [6]u8 = .{ 'g', 'r', 'a', 'y', ' ', 'y' };

    var ss223: [6]u8 = .{ 'g', 'r', 'a', 'y', ' ', 'y' };
    const ss23: []u8 = &ss223; // <-- ref via pointer can be from mutable var only.
    ss23[0] = 'k';

    print("{s} | {s} | {s}\n", .{ ss21, ss22, ss23 });

    const kr1: []const u8 = "안녕";
    print("{s} | {}\n", .{ kr1, kr1.len });

    // ----------------------------------------------
    // struct

    const user: User = .{
        .name = "John",
    };

    // std.debug.print("{}\n", .{user});
    user.toString();
}
