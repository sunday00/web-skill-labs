const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

const he = "Hello";
const wd = "World";

fn catOutVarLen(a: u.String, b: u.String, out: []u8) usize {
    std.debug.assert(out.len >= a.len + b.len);

    std.mem.copyForwards(u8, out, a);
    std.mem.copyForwards(u8, out[a.len..], b);

    return a.len + b.len;
}

fn useCatOutVarLen() void {
    var buf: [128]u8 = undefined;

    const len = catOutVarLen(he, wd, &buf);
    u.print("{}", .{u.eq(he ++ wd, buf[0..len])});

    // make arr and set len little big
    // using std.mem.copy in func
}

fn catOutVarSlice(a: u.String, b: u.String, out: []u8) []u8 {
    std.debug.assert(out.len >= a.len + b.len);

    std.mem.copyForwards(u8, out, a);
    std.mem.copyForwards(u8, out[a.len..], b);

    return out[0 .. a.len + b.len];
}

fn catAlloc(allocator: std.mem.Allocator, a: u.String, b: u.String) ![]u8 {
    const bytes = try allocator.alloc(u8, a.len + b.len);

    std.mem.copyForwards(u8, bytes, a);
    std.mem.copyForwards(u8, bytes[a.len..], b);

    return bytes;
}

fn useCatAlloc() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();
    const resMem = try catAlloc(allocator, he, wd);
    defer allocator.free(resMem);

    u.ssPrint(resMem);
}

pub fn run() !void {
    useCatOutVarLen();

    try useCatAlloc();
}
