const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

pub fn run() !void {
    const a: u8 = 0;
    const a_ptr = &a;

    u.print("{}, {}, {}", .{ a_ptr, a_ptr.*, @TypeOf(a_ptr) });

    var b: u8 = 1;
    const b_ptr = &b;
    b_ptr.* += 1;

    u.print("{}, {}, {}", .{ b_ptr, b_ptr.*, @TypeOf(b_ptr) });

    var arr1 = [_]u8{ 1, 3, 5, 7, 9 };
    var arr2: [*]u8 = &arr1;

    u.print("{any}, {any}, {}", .{ arr1, arr2, @TypeOf(arr2) });

    arr2 += 1;
    u.print("{}, {}, {}, {}, {}", .{ arr1[0], arr2[0], arr2[1], arr2[4], arr2[5] });
    // multi-item pointer arr has not meaning about length like size or count.
    // for being compatible with C lang.

    var arr3 = [_]u8{ 1, 3, 5, 7, 9 };
    arr3[3] = 0;
    const f_ptr: [*:0]const u8 = arr3[0..3 :0];
    u.print("{}, {}, {}", .{ f_ptr[1], @TypeOf(f_ptr), arr3.len });

    const address = @intFromPtr(f_ptr);
    const g_ptr: [*:0]const u8 = @ptrFromInt(address);
    u.print("{}, {any}", .{ address, g_ptr });

    var h_ptr: ?*const usize = null;
    h_ptr = &address;

    u.print("{?}, {}, {}", .{ h_ptr, h_ptr.?.*, @TypeOf(h_ptr) });
    u.print("{}", .{@sizeOf(usize)});
}
