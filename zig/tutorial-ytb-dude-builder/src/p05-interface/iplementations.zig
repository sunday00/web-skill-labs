const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");
const IMember = @import("interface.zig").IMember;

pub const User = struct {
    name: []const u8,

    pub fn readArticle(self: @This(), aid: u8) !void {
        _ = self;

        std.debug.print("read article {}\n", .{aid});
    }

    const VTABLE = struct { // organize implementations function only
        fn implReadArticle(ptr: *anyopaque, aid: u8) !void {
            const self: *User = @ptrCast(@alignCast(ptr)); // anytype to real type

            try self.readArticle(aid);
        }
    };

    pub fn Imember(self: *@This()) IMember {
        return .{ .ptr = self, .name = self.name, .vtable = &.{
            .implReadArticle = VTABLE.implReadArticle,
        } };
    }
};

pub const Admin = struct {
    name: []const u8,

    pub fn readArticle(self: @This(), aid: u8) !void {
        _ = self;

        // other logic

        std.debug.print("admin read article {}\n", .{aid});
    }

    const VTABLE = struct { // organize implementations function only
        fn implReadArticle(ptr: *anyopaque, aid: u8) !void {
            const self: *Admin = @ptrCast(@alignCast(ptr)); // anytype to real type

            try self.readArticle(aid);
        }
    };

    pub fn Imember(self: *@This()) IMember {
        return .{ .ptr = self, .name = self.name, .vtable = &.{
            .implReadArticle = VTABLE.implReadArticle,
        } };
    }
};
