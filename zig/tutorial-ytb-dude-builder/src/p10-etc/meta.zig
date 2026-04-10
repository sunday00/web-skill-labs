const std = @import("std");
const builtin = @import("builtin");

const meta = std.meta;

const Number = union(enum) {
    int: u32,
    float: f32,

    fn is(self: @This(), tag: std.meta.Tag(Number)) bool {
        return self == tag;
    }
};

fn runSimple() !void {
    var n: Number = .{ .int = 11 };
    std.debug.print("{} {} \n", .{ n.is(.int), n.is(.float) });

    n = .{ .float = 1.1 };
    std.debug.print("{} {} \n", .{ n.is(.int), n.is(.float) });

    std.debug.print("{s}\n", .{@tagName(meta.activeTag(n))});

    std.debug.print("{}\n", .{meta.Tag(Number)});
}

fn runChild() void {
    std.debug.print("Element type of []const u8 == {}\n", .{meta.Child([]const u8)});
    std.debug.print("Payload type of ?u8 == {}\n", .{meta.Child(?u8)});

    std.debug.print("[]const u8 -> {}\n", .{meta.Sentinel([]const u8, 0)});

    std.debug.print("Memory layout of Zig struct: {s}\n", .{@tagName(meta.containerLayout(struct { a: u8 }))});
    std.debug.print("Memory layout of Extern struct: {s}\n", .{@tagName(meta.containerLayout(extern struct { a: u8 }))});
    std.debug.print("Memory layout of Packed struct: {s}\n", .{@tagName(meta.containerLayout(packed struct { a: u8 }))});
}

fn runOnOnj() void {
    const fields = meta.fields(struct { a: u8, b: u16, c: f32 });
    // Since this is type data only available at comptime, you
    // have to use and inline for loop.
    inline for (fields) |field| {
        std.debug.print("{s}: {}\n", .{ field.name, field.type });
    }

    const S = struct { a: u8 };
    const s1 = S{ .a = 11 };
    const s2 = S{ .a = 11 };

    // std.debug.print("{}\n", .{s1 == s2}); errrrr
    std.debug.print("{}\n", .{meta.eql(s1, s2)});
    std.debug.print("{?}\n", .{meta.fieldIndex(S, "a")});
}

fn runOnEnum() void {
    const tags = meta.tags(enum { a, b, c });
    std.debug.print("tags[2] == .{s}\n", .{@tagName(tags[2])});

    const FE = meta.FieldEnum(struct { a: u8, b: u16, c: f32 });
    std.debug.print("FE.b: {s}\n", .{@tagName(FE.b)});

    const DE = meta.DeclEnum(struct {
        pub const a: u8 = 0;
        pub const b: u16 = 0;
        pub const c: f32 = 0;
    });
    std.debug.print("DE.b: {s}\n", .{@tagName(DE.b)});
}

fn runOnMath() void {
    const bits = comptime std.math.log2_int_ceil(usize, 1_140_000);
    std.debug.print("{}\n", .{bits});
    std.debug.print("{}\n", .{meta.Int(.unsigned, bits)});
}

fn runOnErr() void {
    var me: anyerror!u8 = error.NotANumber;
    std.debug.print("{}\n", .{meta.isError(me)});

    me = 4;
    std.debug.print("{}\n", .{meta.isError(me)});
}

pub fn main() !void {
    // try runSimple();

    // runChild();

    // runOnOnj();

    // runOnEnum();

    // runOnMath();

    runOnErr();
}
