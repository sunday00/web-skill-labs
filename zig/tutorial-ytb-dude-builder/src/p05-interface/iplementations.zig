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

    fn implReadArticle(ptr: *anyopaque, aid: u8) !void {
        const self: *User = @ptrCast(@alignCast(ptr));

        try self.readArticle(aid);
    }

    const Vtable = struct {
        fn implReadArticle(ptr: *anyopaque, aid: u8) !void {
            const self: *User = @ptrCast(@alignCast(ptr));

            try self.readArticle(aid);
        }
    };

    pub fn Imember(self: *User) IMember {
        return .{ .ptr = self, .name = self.name, .vtable = &.{ .implReadArticle = implReadArticle } };
    }
};

// pub const Admin = struct {
//     name: []const u8,
//
//     pub fn readArticle(self: @This(), aid: u8) !void {
//         _ = self;
//         std.debug.print("admin read anonimuse article {}\n", .{aid});
//     }
//
//     pub fn Imember(self: *Admin) IMember {
//         return .{ .ptr = self, .implReadArticle = Admin.readArticle };
//     }
// };
