const std = @import("std");

fn print(comptime fmt: []const u8, args: anytype) void {
    std.debug.print(fmt, args);
    std.debug.print("\n", .{});
}

fn ssPrint(args: []const u8) void {
    std.debug.print("{s}\n", .{args});
}

pub fn run() !void {
    const e = error{notFound};

    const getUserById = struct {
        fn getUserById(id: u8) ![]const u8 {
            const users = [_][]const u8{ "kim", "lee", "ho" };

            if (id > users.len) return e.notFound;

            return users[id];
        }
    }.getUserById;

    const v = getUserById(7) catch |ee| {
        print("{any}", .{ee});
        return;
    };

    ssPrint(v);
}
