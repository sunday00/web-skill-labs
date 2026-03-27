const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

// fn Point(T: type) type {
fn Point(comptime T: type) type {
    return struct {
        x: T,
        y: T = 0,

        const Self = @This();

        pub fn new(x: T, y: T) Self {
            return .{ .x = x, .y = y };
        }

        pub fn distance(self: Self, other: Self) f64 {
            // const diffx = other.x - self.x;
            // const diffy = other.y - self.y;

            const diffx: f64 = switch (@typeInfo(T)) {
                .Int => @floatFromInt(other.x - self.x),
                .Float => other.x - self.x,
                else => @compileError("number only"),
            };

            const diffy: f64 = switch (@typeInfo(T)) {
                .Int => @floatFromInt(other.y - self.y),
                .Float => other.y - self.y,
                else => @compileError("number only"),
            };

            return @sqrt(diffx * diffx + diffy * diffy);
        }
    };
}

fn typeNameLength(comptime T: type) usize {
    return @typeName(T).len;
}

const A = struct {
    a: u8,
    b: ?u8,

    fn impl() void {}
};

const B = struct {
    a: u8,
    b: ?u8,
};

fn isOptFor(comptime T: type, field_index: usize) bool {
    const fields = @typeInfo(T).Struct.fields;

    inline for (fields, 0..) |field, i| {
        if (field_index == i and @typeInfo(field.type) == .Optional) return true;
    }

    return false;
}

fn isSwitch(comptime T: type, field_index: usize) bool {
    const fields = @typeInfo(T).Struct.fields;

    return switch (field_index) {
        inline 0...field_index - 1 => |idx| @typeInfo(fields[idx].type) == .Optional,
        else => false,
    };
}

const U = union(enum) {
    a: A,
    b: B,

    fn hasImpl(self: U) bool {
        return switch (self) {
            inline else => |s| @hasDecl(@TypeOf(s), "impl"),
        };
    }
};

fn fib(n: usize) usize {
    if (n < 2) return n;

    var a: usize = 0;
    var b: usize = 1;
    var i: usize = 0;

    while (i < n) : (i += 1) {
        const tmp = a;
        a = b;
        b = tmp + b;
    }

    return a;
}

fn chapt1() void {
    const P = Point(f32);

    const p1: P = P.new(1, 1);
    u.print("{}, {}", .{ p1.x, p1.y });
}

fn chapt2() void {
    const condition = false;
    if (condition) {
        @compileError("eeeee");
    }

    const nums = [_]i32{ 2, 4, 6 };
    var sum: usize = 0;

    inline for (nums) |i| {
        const T = switch (i) {
            2 => f32,
            4 => i8,
            6 => bool,
            else => unreachable,
        };

        sum += typeNameLength(T);
    }

    u.print("{any} {}", .{ nums, sum });

    comptime var i: u8 = 0;
    u.dsPrint(i);

    i = 1;
    u.dsPrint(i);

    inline while (i < 4) : (i += 1) {
        const T = switch (i) {
            1 => f32,
            2 => i8,
            3 => bool,
            else => unreachable,
        };

        sum += typeNameLength(T);
    }

    u.print("{}", .{sum});

    u.print("{}", .{fib(7)});

    const ct_fib = comptime blk: {
        break :blk fib(7);
    };
    u.print("{}", .{ct_fib});
}

pub fn run() !void {
    // chapt1();
    chapt2();
}
