const std = @import("std");

fn runOnBools() void {
    const bools_vec_1: @Vector(3, bool) = .{ true, false, true };
    std.debug.print("{any} {} {}\n", .{ bools_vec_1, bools_vec_1[0], @TypeOf(bools_vec_1) });

    const bool_array_1 = [_]bool{ true, false, false };
    const bools_vec_2: @Vector(3, bool) = bool_array_1;
    std.debug.print("{any} {}\n", .{ bools_vec_2, @TypeOf(bools_vec_2) });

    const bools_vec_3 = bools_vec_1 == bools_vec_2;
    std.debug.print("{any} {}\n", .{ bools_vec_3, @TypeOf(bools_vec_3) }); // result => { bv1[0] == bv2[0], bv1[1] == bv2[1], bv1[2] == bv2[2]  }
    const bools_arr_3: [3]bool = bools_vec_3;
    std.debug.print("{any} {}\n", .{ bools_arr_3, @TypeOf(bools_arr_3) }); // cast to array
}

fn runOnInt() void {
    const int_vec_a = @Vector(3, u8){ 1, 2, 3 };
    const int_vec_b = @Vector(3, u8){ 4, 5, 6 };
    const int_vec_c = int_vec_a + int_vec_b;

    std.debug.print("int_vec_c: {any}\n\n", .{int_vec_c});

    const twos: @Vector(3, u8) = @splat(2);
    std.debug.print("{any}\n\n", .{twos});

    const int_vec_d = int_vec_a * twos;
    std.debug.print("int_vec_d: {any}\n\n", .{int_vec_d});
}

fn runWithReduce() void {
    const bv = @Vector(3, bool){ true, true, false };
    const res = @reduce(.And, bv);
    std.debug.print("{}\n", .{res});

    const resAnyTrue = @reduce(.Or, bv);
    std.debug.print("{}\n", .{resAnyTrue});

    const iv = @Vector(3, u8){ 100, 4, 78 };
    const resMax = @reduce(.Max, iv);
    std.debug.print("{}\n", .{resMax});
}

fn runWithShuffle() void {
    const a = @Vector(7, u8){ 'h', 'e', 'l', 'l', 'o', 'o', 'r' };

    const mask1 = @Vector(5, u8){ 0, 1, 2, 3, 4 }; // nothing shuffle. just in-order
    const res1 = @shuffle(u8, a, undefined, mask1);
    std.debug.print("{s}\n", .{@as([5]u8, res1)});

    const mask2 = @Vector(5, u8){ 3, 1, 3, 2, 0 }; // just re order by index no
    const res2 = @shuffle(u8, a, undefined, mask2);
    std.debug.print("{s}\n", .{@as([5]u8, res2)});

    const b = @Vector(4, u8){ 'w', 'd', '!', 'x' };
    const mask3 = @Vector(5, i8){ -1, -2, -3, 1, 2 }; // negative means second group index
    const res3 = @shuffle(u8, a, b, mask3);
    std.debug.print("{s}\n", .{@as([5]u8, res3)});

    // ==== select ====

    const c = @Vector(4, u8){ 'x', 'i', 'j', 'd' };
    const d = @Vector(4, u8){ 's', 'b', 'm', 'z' };
    const mask4 = @Vector(4, bool){ false, true, false, true }; // true 1, false 2
    const res4: @Vector(4, u8) = @select(u8, mask4, c, d);
    std.debug.print("res3: {s}\n\n", .{&@as([4]u8, res4)});
}

fn compare() void {
    const a_vec: @Vector(2, u8) = .{ 'a', 'b' };
    const b_vec: @Vector(2, u8) = .{ 'a', 'b' };

    std.debug.print("{} {}\n", .{ a_vec == b_vec, @reduce(.And, a_vec == b_vec) });

    const c_vec: @Vector(2, u8) = .{ 'a', 'b' };
    const d_vec: @Vector(2, u8) = .{ 'A', 'b' };

    std.debug.print("{} {}\n", .{ c_vec == d_vec, @reduce(.And, c_vec == d_vec) });
}

fn flipAscii(c: u8) u8 {
    return c ^ 0x20;
}

pub fn main() !void {
    // runOnBools();
    // runOnInt();
    // runWithReduce();
    // runWithShuffle();

    const ch1 = flipAscii('a');
    const ch2 = flipAscii('A');
    const ch3 = flipAscii('b');

    std.debug.print("{c} {c} {c} \n", .{ ch1, ch2, ch3 });

    compare();
}
