const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

const he = "Hello";
const wd = "World";

const Error = error{ Boom, OutOfMemory };

const Arena = @import("./arena.zig");

fn logAllocator() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true }).init;
    defer u.print("result: {}", .{gpa.deinit()});

    const allocator = gpa.allocator();

    var list = try Arena.List(u8).init(allocator, 42);
    defer list.deinit();

    try list.append(13);
    try list.append(99);
}

fn logPAllocator() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .verbose_log = true }).init;
    defer u.print("result: {}", .{gpa.deinit()});

    const allocator = gpa.allocator();

    var list = try Arena.AList(u8).init(allocator, 42);
    defer list.deinit();

    try list.append(13);
    try list.append(99);
}

pub fn run() !void {
    try logAllocator();
    try logPAllocator();
}
