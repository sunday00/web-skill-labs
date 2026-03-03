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
        fn getUserById(id: u8) e![]const u8 { // error or return | E!T
            const users = [_][]const u8{ "kim", "lee", "ho" };

            if (id > users.len) return e.notFound;

            return users[id];
        }
    }.getUserById;

    const v = getUserById(7) catch |ee| blk: {
        print("{any}", .{ee});
        // return;
        break :blk "oops";
    };

    ssPrint(v);

    const v2 = getUserById(7);
    if (v2) |res| {
        ssPrint(res);
    } else |err| {
        print("{any}", .{err});
    }

    print("{d}", .{@divTrunc(10, 3)});

    // std.debug.assert(1 == 1);
    // std.debug.assert(1 == 2);
}
