const std = @import("std");
const Allocator = std.mem.Allocator;
const u = @import("../utils.zig");

var allocatorGenenrator = std.heap.GeneralPurposeAllocator(.{}){};
const allocator: Allocator = allocatorGenenrator.allocator();

fn add(comptime T: type, a: T, b: T) !T {
    switch (T) {
        []const u8 => {
            const joined = try std.mem.concat(allocator, u8, &[_][]const u8{ a, b });

            return joined;
        },
        bool => {
            switch (b) {
                true => return a,
                false => return !a,
            }
        },
        else => return a + b,
    }
}

fn cat(a: []const u8, b: []const u8) ![]u8 {
    const joined = try std.mem.concat(allocator, u8, &[_][]const u8{ a, b });

    return joined;
}

pub fn run() !void {
    inline for (0..3) |v| {
        u.print("{d}", .{v});
    }

    u.print("{any}", .{add(u8, 10, 10)});
    u.print("{any}", .{add(bool, true, false)});
    // u.print("{any}", .{add(*const []u8, "hello ", "cat")});
    const cString = try add([]const u8, "hello ", "world");
    u.print("{s}", .{cString});
    defer allocator.free(cString);
}
