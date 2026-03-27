const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

const Point = struct {
    x: f32,
    y: f32 = 0,

    fn new(x: f32, y: f32) Point {
        return .{ .x = x, .y = y };
    }

    fn distance(self: @This(), other: Point) f32 {
        const diffx = other.x - self.x;
        const diffy = other.y - self.y;

        return @sqrt(std.math.pow(f32, diffx, 2) + std.math.pow(f32, diffy, 2));
    }
};

const Namespace = struct {
    const pi: f64 = 3.141592;
    var count: usize = 0;
};

fn setYBaseOnX(x: *f32, y: f32) void {
    const p: *Point = @fieldParentPtr("x", x);

    p.y = y;
}

pub fn run() !void {
    const p1 = Point.new(1, 1);
    const p2: Point = .{ .x = 2, .y = 3 };

    u.print("{d}", .{p1.distance(p2)});
    u.print("{d}", .{Point.distance(p1, p2)});

    var p3 = Point.new(3, 3);
    setYBaseOnX(&p3.x, 11);

    u.print("{}, {}", .{ p3.x, p3.y });

    const cp = &p3;
    u.print("{d:.1}, {d:.1}", .{ cp.y, cp.*.y });
}
