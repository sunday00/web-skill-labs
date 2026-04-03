const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");
const lib = @import("../lib.zig");

// pad space needed for compatible with C
// order preserved
const Extern = extern struct {
    a: u16,
    b: u64,
    c: u16,
};

// pad space needed for fit to memory,
// order random
const Normal = struct {
    a: u16,
    b: u64,
    c: u16,
};

// no pad space, preserve order
const Packed = packed struct {
    a: u16,
    b: u64,
    c: u16,
};

fn printInfo(comptime T: type) void {
    std.debug.print("size of {s}: {}\n", .{ @typeName(T), @sizeOf(T) });

    inline for (std.meta.fields(T)) |field| {
        std.debug.print("  field {s} byte offset: {}\n", .{ field.name, @offsetOf(T, field.name) });
    }

    std.debug.print("\n", .{});
}

pub fn main() !void {
    printInfo(Extern);
    printInfo(Normal);
    printInfo(Packed);
}
