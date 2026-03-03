const std = @import("std");
const u = @import("../utils.zig");

fn optionalReturn(a: u8) ?u8 {
    if (a % 2 == 0) {
        return 1;
    }

    return null;
}

// fn getPointer(sl: []const i32, id: usize) ?*const i32 {
fn getPointer(sl: []i32, id: usize) ?*i32 {
    if (id >= sl.len) return null;

    return &sl[id];
}

pub fn run() !void {
    const el1: ?u8 = optionalReturn(1) orelse 0;

    if (el1 == null) {
        u.ssPrint("a is not even");
    } else {
        u.print("{}\n", .{el1.?}); // .? means definitely not null (like ! in JS).
    }

    var arr1 = [5]i32{ 1, 2, 3, 4, 5 };
    // const ptr: ?*const i32 = getPointer(arr1[0..], 2);
    const ptr: ?*i32 = getPointer(arr1[0..], 2);

    if (ptr == null) @panic("pointer is null");

    u.print("{}\n", .{ptr.?.*});

    ptr.?.* += 1;

    u.print("{}\n", .{ptr.?.*});
    u.print("{any}\n", .{arr1});
}
