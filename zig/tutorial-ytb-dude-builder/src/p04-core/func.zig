const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

fn add(a: u2, b: u2) u2 {
    return a +| b; // <--- not exceed safe u8;
}

fn oops() noreturn { // <---- different with void
    @panic("Ooops");
}

// difference about void and noreturn
// void means "done. I'm back!". main code -> void -> return -> main code
// noreturn means "done. bye~". main code +-> call noreturn code then just continue main code
//                                        |
//                                        +-> run noreturn code on other process
// mainly process exit, forever loop, panic

// not evaluated when the function is not called
fn looseRuntime() void {
    @compileError("never happen");
}

extern "c" fn atan2(a: f64, b: f64) f64; // I thing, it works only always using C lang.

inline fn mul(a: u8, b: u8) u8 { // inline means, full copy and paste inner code to main logic.
    return a *| b; // this means, not just define then call function,
    // copy logic to main code. so, do not use too much long code.
    //
    // short code helps faster, as removing call time delay.
}

fn addOne(n: *u8) void {
    n.* += 1;
}

pub fn run() !void {
    u.dsPrint(add(1, 1)); // 2
    u.dsPrint(add(1, 2)); // u2 = 0, 1, 2, 3 <-- maximum 3
    u.dsPrint(add(2, 3)); // <-- return should be maximum type. 3

    const an = atan2(1, 2);
    u.print("{any}", .{an});

    var p: u8 = 1;
    addOne(&p);

    u.dsPrint(p);
}
