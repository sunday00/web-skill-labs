const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

const Color = enum {
    red,
    green,
    blue,

    fn isRed(self: Color) bool {
        return self == .red;
    }
};

const Num = union { int: u8, float: f64 };

const Token = union(enum) {
    keyword_if,
    keyword_swtch: void,
    digit: usize,

    fn is(self: @This(), tag: std.meta.Tag(Token)) bool {
        return self == tag;
    }
};

pub fn run() !void {
    var fc: Color = .red;
    u.print("{}", .{fc});

    fc = .blue;

    u.print("{} {s} {} {} {}", .{ fc, @tagName(fc), fc.isRed(), @intFromEnum(fc), @as(Color, @enumFromInt(1)) });

    switch (fc) {
        .red => u.ssPrint("red"),
        .green => u.ssPrint("green"),
        .blue => u.ssPrint("blue"),
    }

    var fan: Num = .{ .int = 13 };
    u.nsPrint(.{fan.int});

    fan = .{ .float = 1.1 };
    u.print("{d:.4}", .{fan.float});

    var tok: Token = .keyword_if;
    u.print("{}", .{tok.is(.keyword_if)});

    tok = .{ .digit = 42 };
}
