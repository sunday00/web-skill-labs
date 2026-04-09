const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");
const conf = @import("my_conf");
const clap = @import("zig_clap");

fn usingBuiltinMap(alloc: std.mem.Allocator) !void {
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    const toolMap = std.StringHashMap(u.String);
    var tool = toolMap.init(alloc);
    defer tool.deinit();

    for (args, 0..) |arg, idx| {
        std.debug.print("{} {s}\n", .{ idx, arg });

        if (idx == 0) continue;

        var kv = std.mem.splitScalar(u8, arg, '=');
        const k = kv.next() orelse "";
        const v = kv.next() orelse "";

        try tool.put(k, v);
    }

    std.debug.print("{s} {s}", .{ tool.get("title").?, tool.get("price").? });
}

fn usingZigClap(alloc: std.mem.Allocator) !void {
    // not need to make struct or map
    // just long string parse, follow specific rules.

    const params = comptime clap.parseParamsComptime(
        \\--title <str>...   Something title.
        \\--price <usize>    Something value.
        \\<str>...               Positional arguments.
        \\
    );

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{ .diagnostic = null, .allocator = alloc }) catch |err| {
        try diag.reportToFile(std.fs.File.stderr(), err);
        return err;
    };
    defer res.deinit();

    if (res.args.price) |n|
        std.debug.print("--price = {}\n", .{n});
    for (res.args.title) |s|
        std.debug.print("--title = {s}\n", .{s});

    // for (res.positionals[0]) |pos|
    //     std.debug.print("{s}\n", .{pos});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    // try usingBuiltinMap(alloc);

    try usingZigClap(alloc);
}
