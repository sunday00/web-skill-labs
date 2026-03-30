const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

const testing = std.testing;

fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "basic add functionality" {
    try testing.expect(add(3, 7) == 10);
}

fn sub(a: i32, b: i32) i32 {
    return a - b;
}

test "basic sub functionality" {
    try testing.expect(sub(3, 7) == -4);
}

fn mul(a: i32, b: i32) i32 {
    return a *| b;
}

test "basic mul functionality" {
    try testing.expect(mul(3, 7) == 21);
}

const Foo = struct {
    a: bool,
    b: u8,
    c: []const usize,
    d: u.String,

    const self = @This();

    fn new(flag: bool) Foo {
        return if (flag) .{
            .a = true,
            .b = 1,
            .c = &[_]usize{ 1, 2, 3 },
            .d = "Hello",
        } else .{
            .a = false,
            .b = 0,
            .c = &[_]usize{ 4, 5, 6 },
            .d = "Bye",
        };
    }

    test "inside Foo" {
        try testing.expect(true);
    }
};

test "foo test" {
    const foo = Foo.new(true);
    try testing.expect(foo.a);
    try testing.expectEqual(@as(u8, 1), foo.b);
    try testing.expectEqualSlices(usize, &[_]usize{ 1, 2, 3 }, foo.c);
    try testing.expectEqualStrings("Hello", foo.d);
}

const Error = error{Boom};

fn harmless() Error!void {
    return error.Boom;
}

test "explosive error" {
    try testing.expectError(error.Boom, harmless());
}

test "skip conditionally" {
    if (true) return error.SkipZigTest;

    try testing.expectError(error.Boom, harmless());
}

pub fn run() !void {
    const f1 = Foo.new(true);

    u.print("{any}", .{f1});
}
