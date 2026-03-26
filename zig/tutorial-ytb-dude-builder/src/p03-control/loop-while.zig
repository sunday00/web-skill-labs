const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

fn standard() void {
    var i: usize = 0;
    while (i < 3) {
        u.dsPrint(i);

        i += 1;
    }
}

fn oneLine() void {
    var i: usize = 0;
    while (i < 3) : (i += 1) u.dsPrint(i);
}

fn complexLine() void {
    var i: usize = 0;
    var j: usize = 0;

    while (i + j < 20) : ({
        i += 1;
        j += 2;
    }) u.dsPrint(i + j);
}

fn labelAndBreak() void {
    var i: usize = 0;
    outer: while (i < 100) : (i += 1) {
        var j: usize = 0;
        inner: while (j < 100) : (j += 1) {
            if (j % 2 == 0) continue :inner;
            if (i * j > 30) break :outer;

            u.nsPrint(.{ i, j, i * j });
        }
    }
}

fn whileExpression(x: u8) bool {
    var i: usize = 0;
    const pass = while (i < 100) : (i += 1) {
        if (x == i) break true;
    } else false;

    return pass;
}

fn countDown() ?usize {
    return if (count == 0) null else blk: { // <-----------------------------+
        count -= 1; //                                                       |
        break :blk count; //                                                 |
    }; //                                                                    |
} //                                                                         |
//                                                                           |
fn capt() void { //                                                          |
    count = 3; // <----------------------------------------------------------+
    while (countDown()) |num| u.dsPrint(num); //                      |
} //                                                                         |
//                                                                           |
var count: usize = undefined; // defining variable after function is fine. >-+

fn arr() void {
    const arrr = [_]u8{ 1, 2, 3, 4, 5 };
    // while (arrr) |x| u.dsPrint(x); // not work! // arrr should be OPTIONAL
    for (arrr) |x| u.dsPrint(x);
}

pub fn run() !void {
    standard();
    oneLine();
    complexLine();

    labelAndBreak();

    u.print("{}", .{whileExpression(11)});

    capt();

    arr();
}
