const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

pub fn run() !void {
    u.print("\t \' \x65 \u{e9}", .{});

    const hello = "Hello";
    const bye = "Bye";

    u.print("{0c} {1u}", .{ hello[0], bye[0] });
    //                                  ^           ^
    //                                  |           |
    // 0, 1, means argument index ------+-----------+
    // c : ascii / u : unicode

    var hello_acute: []const u8 = "h\xe9llo";
    u.print("{s} {} {}", .{ hello_acute, hello_acute.len, std.unicode.utf8ValidateSlice(hello_acute) });

    hello_acute = "h\u{e9}llo";
    u.print("{s} {} {}", .{ hello_acute, hello_acute.len, std.unicode.utf8ValidateSlice(hello_acute) });

    const mul =
        \\ this is long
        \\ sentence
        \\ with string...;;
    ;

    u.ssPrint(mul);

    const iguana = '🦎';
    const bot: u21 = '🤖';
    u.print("{u}, {u}", .{ iguana, bot });

    u.print("{}", .{std.ascii.isUpper(hello[0])});
}
