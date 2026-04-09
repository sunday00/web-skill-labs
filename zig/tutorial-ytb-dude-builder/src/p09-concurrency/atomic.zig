const std = @import("std");

const WaitGroup = struct {
    members: usize = 0,

    fn init() WaitGroup {
        var wg = WaitGroup{};

        _ = @atomicStore(usize, &wg.members, 0, .seq_cst);

        return wg;
    }

    fn add(self: *@This()) void {
        _ = @atomicRmw(usize, &self.members, .Add, 1, .seq_cst);
    }

    fn done(self: *@This()) void {
        _ = @atomicRmw(usize, &self.members, .Sub, 1, .release);
    }

    fn wait(self: *@This()) void {
        while (@atomicLoad(usize, &self.members, .monotonic) > 0) {
            std.Thread.sleep(500 * std.time.ns_per_ms);
        }
    }
};

fn worker(id: usize, wg: *WaitGroup) void {
    defer wg.done();

    std.debug.print("{} started \n", .{id});

    std.Thread.sleep(1 * std.time.ns_per_ms);

    std.debug.print("{} finished \n", .{id});
}

pub fn main() !void {
    var wg = WaitGroup.init();
    defer wg.wait();

    for (0..5) |i| {
        wg.add();

        const thread = std.Thread.spawn(.{}, worker, .{ i, &wg }) catch |err| {
            wg.done();
            return err;
        };

        thread.detach();
    }
}
