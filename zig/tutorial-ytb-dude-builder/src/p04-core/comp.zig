const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

// fn Point(T: type) type {
fn Point(comptime T: type) type {
    return struct {
        x: T,
        y: T = 0,

        const Self = @This();

        pub fn new(x: T, y: T) Self {
            return .{ .x = x, .y = y };
        }

        pub fn distance(self: Self, other: Self) f64 {
            // const diffx = other.x - self.x;
            // const diffy = other.y - self.y;

            const diffx: f64 = switch (@typeInfo(T)) {
                .Int => @floatFromInt(other.x - self.x),
                .Float => other.x - self.x,
                else => @compileError("number only"),
            };

            const diffy: f64 = switch (@typeInfo(T)) {
                .Int => @floatFromInt(other.y - self.y),
                .Float => other.y - self.y,
                else => @compileError("number only"),
            };

            return @sqrt(diffx * diffx + diffy * diffy);
        }
    };
}

pub fn run() !void {
    const P = Point(f32);

    const p1: P = P.new(1, 1);
    u.print("{}, {}", .{ p1.x, p1.y });
}
