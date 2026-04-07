const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

pub const IMember = struct {
    ptr: *anyopaque,

    name: []const u8,
    vtable: *const Vtable,

    pub const Vtable = struct {
        implReadArticle: *const fn (ptr: *anyopaque, aid: u8) anyerror!void,
        // implUpdateName: *const fn (*anyopaque, name: []const u8) anyerror![]u8,
    };

    pub fn readArticle(self: IMember, aid: u8) anyerror!void {
        try self.vtable.implReadArticle(self.ptr, aid);
    }

    // pub fn updateName(self: @This(), name: []const u8) anyerror![]const u8 {
    //     return self.implUpdateName(self.ptr, name);
    // }
};
