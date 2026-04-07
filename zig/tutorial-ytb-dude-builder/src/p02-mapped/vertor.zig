const std = @import("std");

fn runOnBools() void {
    const bools_vec_1: @Vector(3, bool) = .{ true, false, true };
    std.debug.print("{any} {}\n", .{ bools_vec_1, @TypeOf(bools_vec_1) });

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

pub fn main() !void {
    runOnInt();
}
