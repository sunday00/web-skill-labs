const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");
const lib = @import("../lib.zig");

const Allocator = std.mem.Allocator;

const User = struct {
    allocator: Allocator,
    id: usize,
    email: u.mString,

    fn init(alloc: Allocator, id: usize, email: u.String) !User {
        return .{
            .allocator = alloc,
            .id = id,
            .email = try alloc.dupe(u8, email),
        };
    }

    fn deinit(self: @This()) void {
        self.allocator.free(self.email);
    }

    pub fn format(self: @This(), writer: anytype) !void {
        try writer.print("[User.fomat] id: {}, email: {s}", .{ self.id, self.email });
    }
};

const UserData = struct {
    map: std.AutoHashMap(u.ID, User),

    fn init(alloc: Allocator) UserData {
        return .{ .map = std.AutoHashMap(u.ID, User).init(alloc) };
    }

    fn deinit(self: *UserData) void {
        self.map.deinit();
    }

    fn put(self: *UserData, user: User) !void {
        try self.map.put(user.id, user);
    }

    fn findOrCreate(self: *UserData, user: User) !void {
        const pointer = try self.map.getOrPut(user.id);
        if (!pointer.found_existing) pointer.value_ptr.* = user;
    }

    fn get(self: UserData, id: u.ID) ?User {
        return self.map.get(id);
    }

    fn containsKey(self: *UserData, id: u.ID) void {
        u.print("{}", .{self.map.contains(id)});
    }

    fn del(self: *UserData, id: u.ID) ?User {
        return if (self.map.fetchRemove(id)) |kv| kv.value else null;
    }

    fn utils(self: *UserData) void {
        u.print("{}", .{self.map.count()});
    }

    fn debugPrint(self: *UserData) void {
        var iter = self.map.iterator();
        while (iter.next()) |user| {
            u.print("[UserData.debugPrint] mapKey: {} {f}", .{ user.key_ptr.*, user.value_ptr.* });
        }
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var users = UserData.init(alloc);
    defer users.deinit();

    const userNames = [_]u.String{ "jeff", "alice", "bob" };
    var namesForAllocator = std.heap.ArenaAllocator.init(alloc);
    defer namesForAllocator.deinit();
    const namesForAlloc = namesForAllocator.allocator();

    inline for (userNames, 0..) |name, id| {
        const email: u.String = name ++ "@foo.io";
        const user = try User.init(namesForAlloc, id, email);

        try users.put(user);
    }

    users.debugPrint();

    const delU = users.del(0);
    u.print("{f}", .{delU.?});

    users.debugPrint();

    users.utils();
    users.containsKey(1);

    const sam = try User.init(namesForAlloc, 3, "sam@bar.io");
    try users.findOrCreate(sam);

    users.debugPrint();

    if (users.get(2)) |no2| {
        u.print("{f}", .{no2});
    }

    const tom = try User.init(namesForAlloc, 2, "tom@bar.io");
    try users.put(tom);
    users.debugPrint();
}
