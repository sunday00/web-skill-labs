const std = @import("std");
const rl = @import("raylib");

const global = @import("../_common/global.zig");

const Schema = struct {
    id: u8,
    value: []u8,

    pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.print("id: {}, value: {s}\n", self);
    }
};

pub fn run() !void {
    var buff: [4096]u8 = undefined;

    var threaded: std.Io.Threaded = .init_single_threaded;
    const io = threaded.io();

    var client = std.http.Client{ .allocator = global.g.alloc, .io = io };
    defer client.deinit();

    const uri = try std.Uri.parse("https://my-json-server.typicode.com/sunday00/placeholders/metaVariables/1");
    var req = try client.request(.GET, uri, .{});
    defer req.deinit();

    try req.sendBodiless();

    var res = try req.receiveHead(&.{});
    const body = try res.reader(&buff).readAlloc(global.g.alloc, @intCast(res.head.content_length orelse 0));
    defer global.g.alloc.free(body);

    const example = try std.json.parseFromSlice(Schema, global.g.alloc, body, .{ .ignore_unknown_fields = true });
    defer example.deinit();

    std.debug.print("{f}\n", .{example.value});

    while (!rl.windowShouldClose()) {
        rl.beginDrawing();
        defer rl.endDrawing();

        rl.drawText("hello test", 20, global.g.screenH - 100, 20, rl.Color.white);
    }
}
