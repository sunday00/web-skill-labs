const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

var buff: [4096]u8 = undefined;
var bw = std.fs.File.stdout().writer(&buff);
const stdout = &bw.interface;

const S = struct {
    isBoy: bool = true,
    rat: f16 = 3.1415,
};

fn usingStdOut() !void {
    try stdout.print("abcdefg {s} hijklmnop\n\n", .{"HOOO~~!"});

    const float: f64 = 3.1415;
    try stdout.print("{} {0d} {0d:0<10.2} {0d:0^10.2} {0d:0>10.2} \n\n", .{float});

    const ii: u8 = 42;
    // try stdout.print("{} {b} {o} {x} {c} {u} \n\n", .{ii});
    try stdout.print("{}  ", .{ii});
    try stdout.print("{b}  ", .{ii}); // binary
    try stdout.print("{o}  ", .{ii}); // octo 8
    try stdout.print("{x}  ", .{ii}); // hex 16
    try stdout.print("{c}  ", .{ii}); // ascii
    try stdout.print("{u}  \n\n", .{ii}); // unicode

    const iToS: u.String = try std.fmt.bufPrint(&buff, "{}", .{ii});
    try stdout.print("{s}  \n\n", .{iToS});
}

fn stringPart() !void {
    const s = "Hello warrior";

    try stdout.print("!{s}!    !{0s:^21}!    !{0s:_^21}!    !{0s:<21}!    !{0s:>21}! \n\n", .{s});
}

fn printOptional() !void {
    const o: ?u8 = 42;

    try stdout.print("{?} \n\n", .{o});
    try stdout.print("{?} \n\n", .{@as(?u8, null)});

    try stdout.print("!{?d:0>10}! \n\n", .{o});
    try stdout.print("!{?d:0>10}! \n\n", .{@as(?u8, null)});
}

fn throwable() !void {
    const errorU: anyerror!u8 = error.WrongNumber;
    try stdout.print("{!} {!} \n\n", .{ errorU, @as(anyerror!u8, 13) });
    try stdout.print("{!d:0>10} \n\n", .{@as(anyerror!u8, 13)});
}

fn printPointer() !void {
    const float: f64 = 3.1415;
    const pt = &float;

    try stdout.print("{} {0*} {} \n\n", .{ pt, pt.* });
}

fn printProperty() !void {
    const s = S{};

    try stdout.print("{[isBoy]} {[rat]d:.2} \n\n", s); // args is not .{s}, just s ⚠️

}

fn usingAlloc() !void {
    const s = S{};

    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const str_alloc = try std.fmt.allocPrint(allocator, "{[isBoy]} {[rat]} \n\n", s);
    defer allocator.free(str_alloc);

    try stdout.print("{s}", .{str_alloc});

    // curly in curly for escape curly
    try stdout.print("{{s}}", .{});
}

pub fn main() !void {
    try usingStdOut();

    try stringPart();

    try printOptional();

    try throwable();

    try printPointer();

    try printProperty();

    try usingAlloc();

    try stdout.flush();
}
