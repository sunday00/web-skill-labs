const std = @import("std");

const Counter = struct {
    count: u8 = 0,

    fn increment(self: *@This()) void {
        const before = self.count;
        std.Thread.sleep(250 * std.time.ns_per_ms);
        self.count +%= 1;
        std.debug.print("wc {} -> {} \n", .{ before, self.count });
    }

    fn print(self: *@This()) void {
        std.debug.print("rc {}\n", .{self.count});
    }
};

const MCounter = struct {
    locker: std.Thread.Mutex = .{},
    count: u8 = 0,

    fn increment(self: *@This()) void {
        self.locker.lock(); // completely lock
        defer self.locker.unlock();

        const before = self.count;
        std.Thread.sleep(250 * std.time.ns_per_ms);
        self.count +%= 1;
        std.debug.print("wc {} -> {} \n", .{ before, self.count });
    }

    fn print(self: *@This()) void {
        self.locker.lock(); // completely lock
        defer self.locker.unlock();

        std.debug.print("rc {}\n", .{self.count});
    }
};

const RWCounter = struct {
    locker: std.Thread.RwLock = .{},
    count: u8 = 0,

    fn increment(self: *@This()) void {
        self.locker.lock(); // completely lock
        defer self.locker.unlock();

        const before = self.count;
        std.Thread.sleep(250 * std.time.ns_per_ms);
        self.count +%= 1;
        std.debug.print("wc {} -> {} \n", .{ before, self.count });
    }

    fn print(self: *@This()) void {
        self.locker.lockShared(); // write lock only
        defer self.locker.unlock();

        std.debug.print("rc {}\n", .{self.count});
    }
};

// fn incrementCount(counter: *Counter) void {
// fn incrementCount(counter: *MCounter) void {
fn incrementCount(counter: *RWCounter) void {
    while (true) {
        std.Thread.sleep(500 * std.time.ns_per_ms);
        counter.increment();
    }
}

// fn printCounter(counter: *Counter) void {
// fn printCounter(counter: *MCounter) void {
fn printCounter(counter: *RWCounter) void {
    while (true) {
        std.Thread.sleep(250 * std.time.ns_per_ms);
        counter.print();
    }
}

pub fn main() !void {
    // var counter = Counter{};
    // var counter = MCounter{};
    var counter = RWCounter{};
    //
    for (0..10) |_| {
        var thread = try std.Thread.spawn(.{}, incrementCount, .{&counter});
        thread.detach();
    }

    for (0..10) |_| {
        var thread = try std.Thread.spawn(.{}, printCounter, .{&counter});
        thread.detach();
    }

    std.Thread.sleep(4 * std.time.ns_per_s);
    std.debug.print("\n", .{});
}
