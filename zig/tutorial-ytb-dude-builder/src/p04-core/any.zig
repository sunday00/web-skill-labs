const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

const A = struct {
    fn toString(_: A) u.String {
        return "A";
    }
};

const B = struct {
    fn toString(s: u.String) u.String {
        return s;
    }
};

const C = struct {
    const toString: u.String = "C";
};

const D = enum {
    a,
    d,
    fn toString(self: D) u.String {
        return @tagName(self);
    }
};

fn PRINT(x: anytype) void {
    const T = @TypeOf(x);
    if (!@hasDecl(T, "toString")) return;

    const declType = @TypeOf(@field(T, "toString"));

    if (@typeInfo(declType) != .@"fn") return; // is function check...

    const args = std.meta.ArgsTuple(declType);
    inline for (std.meta.fields(args), 0..) |arg, i| {
        if (i == 0 and arg.type == T) {
            u.ssPrint(x.toString());
        }
    }
}

pub fn run() !void {
    const a = A{};
    const b = B{};
    const c = C{};
    const d = D.d;

    PRINT(a);
    PRINT(b);
    PRINT(c);
    PRINT(d);
}
