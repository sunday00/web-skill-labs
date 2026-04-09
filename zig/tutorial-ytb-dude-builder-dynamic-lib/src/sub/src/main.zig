const std = @import("std");
const testing = std.testing;

export fn calc(a: i32, b: i32) i32 {
    std.debug.print("sub .... {} {} \n", .{a, b});
    return sub(a, b);
}

fn sub(a: i32, b: i32) i32 {
    return a - b;
}

test "basic sub functionality" {
    try testing.expect(sub(3, 7) == -4);
}
