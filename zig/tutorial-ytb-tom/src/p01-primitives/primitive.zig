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

    pub fn format(
        self: User,
        // self: @This(),
        // self: *const User,
        // writer: *std.io.Writer,
        writer: anytype,
    ) !void {
        // _ = fmt;
        // _ = options;

        try writer.print("User: {s} ({d}, {s})", .{
            self.name,
            self.age,
            @tagName(self.occupation),
        });
    }

    // pub fn print(self: @This()) void {
    pub fn print(self: *const User) void {
        std.debug.print("User: {s} ({d}, {s})", .{
            self.name,
            self.age,
            @tagName(self.occupation),
        });
    }
};

const ExampleUnion = union(enum) {
    bb: i64,
    ss: i8,
    boo: bool,
    // pub fn format(self: *const ExampleUnion, writer: anytype) !void {
    pub fn format(self: @This(), writer: anytype) !void {
        switch (self) {
            .bb, .ss => |val| try writer.print("{d}", .{val}),
            // else => {},
            .boo => |val| try writer.print("{}", .{val}),
        }
    }
};

const EEE = enum { bb, ss, user };

const ExampleUnion2 = union(EEE) {
    bb: i64,
    ss: i8,
    user: User,
    // pub fn format(self: *const ExampleUnion, writer: anytype) !void {
    pub fn format(self: @This(), writer: anytype) !void {
        switch (self) {
            .bb, .ss => |val| try writer.print("{d}", .{val}),
            // else => {},
            .user => |val| try writer.print("{s}", .{val.name}),
        }
    }
};

fn printString(comptime input: []const u8) void {
    std.debug.print(input, .{});
}

fn add(a: i32, b: i32) i32 {
    return a + b;
}

pub fn run() !void {
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

    std.debug.print("{f}\n", .{user});
    // user.print();

    // ----------------------------------------------
    // union?

    const ue = ExampleUnion{ .bb = 10 };
    print("{f}\n", .{ue});

    const ue2 = ExampleUnion2{ .bb = 10 };
    print("{f}\n", .{ue2});

    switch (ue2) {
        .bb, .ss => |v| print("{}\n", .{v}),
        .user => |v| print("{s}\n", .{v.name}),
    }

    var ue22 = ExampleUnion2{ .user = user };
    print("{f}\n", .{ue22});

    ue22 = ExampleUnion2{ .bb = 11 };
    print("{f}\n", .{ue22});
}
