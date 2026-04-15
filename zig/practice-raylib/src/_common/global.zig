const std = @import("std");

pub const G = struct {
    alloc: std.mem.Allocator,
    screenW: u16,
    screenH: u16,
};

pub var g: G = .{
    .alloc = undefined,
    .screenW = undefined,
    .screenH = undefined,
};
