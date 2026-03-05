const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

pub fn run() !void {
    u.print("{} {}", .{ @intFromBool(true), @intFromBool(false) });
    // u.print("{} {}", .{ @as(bool, 1), @as(bool, 0) }); // inverse not work. But...
    u.print("{} {}", .{ 1 == 1, 0 == 1 }); // use {== 1} could be easily convert int to bool

    var sayBye: u8 = undefined;
    var nullableVar1: ?u8 = null;
    {
        nullableVar1 = 1;
        // sayBye = nullableVar1; // <------ sayBye is not nullable. runtime err
        {
            nullableVar1 = 100;
            sayBye = nullableVar1.?; // .? unwrap.
        }

        nullableVar1 = 2;
        sayBye = 3;

        u.print("{} {}", .{ nullableVar1.?, sayBye });
    }

    sayBye = nullableVar1 orelse 100;
    u.print("{} {}", .{ nullableVar1.?, sayBye });

    // ----- describe capturing

    var nullableVar2: ?u8 = 88;
    if (nullableVar2) |v| {
        u.print("{}", .{v + 1});
    } else {
        nullableVar2 = 1;
    }

    // ------- alternate A ? B : C ----

    const x = 8;
    const target = if (x < 5) x else 1;
    u.print("{}", .{target});
}
