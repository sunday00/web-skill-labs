const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");
const lib = @import("../lib.zig");

fn printCharList(T: type, l: std.ArrayList(T)) !void {
    std.debug.print("check inside: [", .{});

    for (l.items) |value| {
        std.debug.print("{c}", .{value});
    }

    std.debug.print("] \n\n", .{});
}

fn basic(alloc: std.mem.Allocator, T: type) !std.ArrayList(T) {
    var l: std.ArrayList(T) = .empty;

    // try l.append(alloc, "hello");
    for ("hello, World!") |byte| try l.append(alloc, byte);

    return l;
}

fn usingWriter(T: type, alloc: std.mem.Allocator, l: *std.ArrayList(T)) !void {
    const w = l.writer(alloc);
    try w.print("?, {s}", .{"Huh?"});
}

fn usingPopIter(T: type, l: *std.ArrayList(T)) !void {
    while (l.pop()) |b| {
        std.debug.print("{c}", .{b});
    }

    std.debug.print("\n\n", .{});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var l1 = try basic(alloc, u8);
    defer l1.deinit(alloc);

    try printCharList(u8, l1);

    _ = l1.pop();

    try printCharList(u8, l1);

    try usingWriter(u8, alloc, &l1);

    try printCharList(u8, l1);

    try usingPopIter(u8, &l1);

    try printCharList(u8, l1);

    // TODO: appendSlice
}
