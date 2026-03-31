const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

// global constant text section
// stored into .rodata
// app init -> app terminated
// immutable
const pi: f64 = 3.1415;
const greeting = "Hello";

// global data section
// stored into .data - defined.
var count: usize = 0;

// global data section
// stored into .bss - undefined.
var count2: usize = undefined;

// global variables are keep life until end of app

fn locals() u8 {
    // local variables are into stackFrame.
    // call func -> end of scope

    var a: u8 = 1;
    var b: u8 = 2;
    var result: u8 = a + b;

    a = 2;
    b = 3;
    result = a + b;

    // primitive return

    // // // original result value on memory to be gone.
    return result;
    // // // so, returning value is CLONE from original value.
}

// 🔴 bad idea
fn badIdeal() *u8 {
    // it looks working.
    // but let's think about this.
    //
    // x is gone at the end of scope.
    var x: u8 = 42;

    // returned value point to x-origin-memory which will be empty.
    return &x;
    // this is what we called 'dangling err'
}

// 🔴 bad idea
fn badIdeal2() []u8 {
    var arr = [_]u8{ 'H', 'e', 'l', 'l', 'o' };
    var sl = arr[2..];
    sl = arr[2..];

    return sl;

    // hard to raise err
    // but slice is pointer too.
    // this cause dangling err too.
}

// 🟢 good idea
fn goodIdea(allocator: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
    var arr = [_]u8{ 'H', 'e', 'l', 'l', 'o' };
    const sl = try allocator.alloc(u8, 3);

    std.mem.copyBackwards(u8, sl, arr[2..]);

    // this memory allocator is not born from local.
    // so, variable points to not in this func,
    // point to memory which is on outside.
    return sl;
}

const Foo = struct {
    s: []u8,

    fn init(allocator: std.mem.Allocator, s: u.String) !*Foo {
        const foo_ptr = try allocator.create(Foo);
        errdefer allocator.destroy(foo_ptr);

        foo_ptr.s = try allocator.alloc(u8, s.len);
        std.mem.copyForwards(u8, foo_ptr.s, s);
        // Or: foo_ptr.s = try allocator.dupe(s);

        return foo_ptr;
    }

    fn deinit(self: *Foo, allocator: std.mem.Allocator) void {
        // `free` works on slices allocated with `alloc`.
        allocator.free(self.s);
        // `destroy` works on pointers allocated with `create`.
        allocator.destroy(self);
    }
};

pub fn run() !void {
    count2 = 1;

    u.print("{}", .{pi});

    const ll = locals();
    u.print("{}", .{ll});
    u.print("{}", .{ll});
    u.print("{}", .{ll});

    const bb = badIdeal();
    u.print("{}", .{bb.*}); // <-- maybe luckily able to use 42 if os has not overwritten the memory
    u.print("{}", .{bb.*}); // <-- high probability that this memory will be empty. result should be 'empty 0'
    u.print("{}", .{bb.*}); // <-- empty 0

    const bb2 = badIdeal2();
    u.print("{any}", .{bb2});

    var gpa = std.heap.GeneralPurposeAllocator(.{
        .safety = true, // true on dev
    }).init; // get allocator instance.
    defer _ = gpa.deinit(); // booking clear the instance itself
    const allocator = gpa.allocator(); // get allocator interface

    // const allocator = std.heap.page_allocator;

    //
    const gg1 = try goodIdea(allocator);
    defer allocator.free(gg1);

    // remember
    //
    // init and get instance allocator
    // defer instance de init itself
    // get interface
    // pass interface to func
    // defer memory clear

    u.print("{any}", .{gg1});

    var foo = try Foo.init(allocator, "hello");
    defer foo.deinit(allocator);

    u.print("{s}", .{foo.s});
}
