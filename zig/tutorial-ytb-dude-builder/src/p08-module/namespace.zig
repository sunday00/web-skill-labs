const std = @import("std");

const pi: f64 = 3.1415;
var x: u8 = 42;

const AliasStd = struct {
    // usingnamespace std;  // no. after 0.15.1, remove usingnamespace.
};

fn greet() void {
    std.debug.print("hello\n", .{});
}

const Foo = struct {
    const pi: f64 = 3.1415;
    var x: u8 = 42;

    fn greet() void {
        std.debug.print("hello\n", .{});
    }
};

const Bar = struct {
    const pi: f64 = 3.1415;
    var x: u8 = 42;

    fn greet() void {
        std.debug.print("hello\n", .{});
    }
};

pub fn main() !void {}
