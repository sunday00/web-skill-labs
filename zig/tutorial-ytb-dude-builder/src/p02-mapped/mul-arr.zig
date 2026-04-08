const std = @import("std");

const Foo = struct {
    a: u16,
    b: u64,
    c: u16,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var mul = std.MultiArrayList(Foo){};
    defer mul.deinit(alloc);

    try mul.append(alloc, .{ .a = 1, .b = 2, .c = 3 });

    std.debug.print("{} {} {}\n", mul.get(0)); // get horizontal values
    std.debug.print("{} {} \n", .{ mul.capacity, mul.len }); // 16

    try mul.ensureUnusedCapacity(alloc, 2); // plus "AT LEAST 2 more able to append". not exactly 2 spaces.;
    std.debug.print("{} {} \n", .{ mul.capacity, mul.len }); // 16 // ? not increased. because, list already enough to add 2 more length.

    try mul.ensureUnusedCapacity(alloc, 20); // plus more 20 able to capacity assume.;
    std.debug.print("{} {} \n", .{ mul.capacity, mul.len }); // 40 // not 36. again, plus ensured capacity, not exactly 20, at least 20 more fluently space added.

    mul.appendAssumeCapacity(.{ .a = 11, .b = 12, .c = 13 }); // alloc memory already ensured, so, developer doesn't need to alloc each.
    std.debug.print("{} {} \n", .{ mul.capacity, mul.len });

    mul.appendAssumeCapacity(.{ .a = 21, .b = 22, .c = 33 });
    std.debug.print("{} {} \n", .{ mul.capacity, mul.len });

    std.debug.print("{any}\n", .{mul.items(.a)}); // vertical values

    const sl = mul.slice();
    std.debug.print("{any}\n", .{sl});
    for (0..sl.len) |idx| {
        std.debug.print("{}\n", .{sl.get(idx)});
    }

    mul.set(1, .{ .a = 0, .b = 0, .c = 0 });
    std.debug.print("{any}\n", .{mul.items(.a)}); // vertical values
}
