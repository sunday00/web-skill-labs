const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");
const String = @import("zig_string").String;

pub fn main() !void {
    std.debug.print("{s}\n", .{"hello"});

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var myString = String.init(arena.allocator());
    defer myString.deinit();

    try myString.concat("🔥 Hello!");
    _ = myString.pop();
    try myString.concat(", World 🔥");

    std.debug.print("{s}", .{myString.str()});
}
