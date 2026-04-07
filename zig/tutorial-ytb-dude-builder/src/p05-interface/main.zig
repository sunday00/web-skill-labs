const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");

const IMember = @import("interface.zig").IMember;
const Members = @import("iplementations.zig");

fn readArticleAnyRole(member: IMember, aid: u8) !void {
    try member.readArticle(aid);
}

pub fn main() !void {
    var user = Members.User{
        .name = "Kelbin",
    };

    try user.readArticle(1);

    // var admin = Members.Admin{
    //     .name = "Administrtor",
    // };

    // try admin.readArticle(1);

    try readArticleAnyRole(user.Imember(), 2);
    // try readArticleAnyRole(admin.Imember(), 2);
}
