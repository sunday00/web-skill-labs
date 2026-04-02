const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

fn usingStdOut() !void {
    // const f = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(f);
    // const stdout = bw.writer();
    //
    // try stdout.print("abcdefg\n\n", .{});

    var buff: [4096]u8 = undefined;
    var bw = std.fs.File.stdout().writer(&buff);
    const stdout = &bw.interface;

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

    try stdout.flush();
}

fn stringPart() !void {
    var buff: [4096]u8 = undefined;
    var bw = std.fs.File.stdout().writer(&buff);
    const stdout = &bw.interface;

    const s = "Hello warrior";

    try stdout.print("!{s}!    !{0s:^21}!    !{0s:_^21}!    !{0s:<21}!    !{0s:>21}! \n\n", .{s});

    try stdout.flush();
}

fn printOptional() !void {
    var buff: [4096]u8 = undefined;
    var bw = std.fs.File.stdout().writer(&buff);
    const stdout = &bw.interface;

    const o: ?u8 = 42;

    try stdout.print("{?} \n\n", .{o});
    try stdout.print("{?} \n\n", .{@as(?u8, null)});

    try stdout.print("!{?d:0>10}! \n\n", .{o});
    try stdout.print("!{?d:0>10}! \n\n", .{@as(?u8, null)});

    try stdout.flush();
}

pub fn main() !void {
    try usingStdOut();

    try stringPart();

    try printOptional();
}
