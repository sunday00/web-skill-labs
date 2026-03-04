const std = @import("std");
const print = std.debug.print;

fn println(comptime fmt: []const u8, args: anytype) void {
    print(fmt, args);
    print("\n", .{});
}

const MIN_MAX = struct { min: u8 = 0, max: u8 = 100 };

fn simpleRand(minmax: MIN_MAX) u8 {
    // 1. 시드 생성 (현재 시간 기반)
    var seed: u64 = undefined;
    std.posix.getrandom(std.mem.asBytes(&seed)) catch {
        seed = 0;
    };

    // 2. 난수 생성기 초기화 (Default PRNG: Xoshiro256)
    var prng = std.Random.DefaultPrng.init(seed);
    const random = prng.random();

    // 3. 1부터 100 사이의 숫자 생성
    // uintAtMost(타입, 최댓값) -> 0 ~ 100 생성
    // 거기에 + 1을 하면 1 ~ 100이 됩니다.
    return random.uintAtMost(u8, minmax.max - minmax.min) + minmax.min;
}

fn isAlphabet(a: u8) bool {
    switch (a) {
        'a'...'z' => return true,
        'A'...'Z' => return true,
        else => return false,
    }
}

pub fn run() !void {
    // -------------- conditions

    if (1 == 1) {
        println("sleepy\n", .{});
        println("sleepy\n", .{});
    }

    const ii = 3;
    switch (ii) {
        1 => print("1\n", .{}),
        2 => print("2\n", .{}),
        else => print("3\n", .{}),
    }

    const rn = simpleRand(MIN_MAX{ .min = 0, .max = 100 });
    println("{d}", .{rn});

    switch (rn) {
        0 => println("zero", .{}),
        1, 2, 3, 4, 5 => println("tiny", .{}),
        6...30 => println("small", .{}),
        31...65 => println("medium", .{}),
        else => {
            println("big", .{});
        },
    }

    println("{}, {}", .{ isAlphabet('A'), 'A' });
    println("{}, {}", .{ isAlphabet('Z'), 'Z' });
    println("{}, {}", .{ isAlphabet('a'), 'a' });
    println("{}, {}", .{ isAlphabet('z'), 'z' });
    println("{}, {}", .{ isAlphabet(125), 125 });
}
