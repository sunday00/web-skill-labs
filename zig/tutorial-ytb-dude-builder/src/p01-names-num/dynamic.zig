const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");
const lib = @import("../lib.zig");

// avoid keyword interrupt
// connecting with external c, c++
pub fn run() !void {
    const @"white space" = 123;
    u.print("{}", .{@"white space"});

    const @"try if you catch for" = "wow";
    u.print("{s}", .{@"try if you catch for"});

    const @"try" = true;
    u.print("{}", .{@"try"});
}
