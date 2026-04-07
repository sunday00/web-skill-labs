const std = @import("std");
const builtin = @import("builtin");
const u = @import("../custom_utils.zig");
const lib = @import("../lib.zig");

const math = @cImport({
    @cDefine("INCREMENT_BY", "10");
    @cInclude("math.h");
});

// https://www.youtube.com/watch?v=dHmw-424CMY&list=PLtB7CL7EG7pCw7Xy1SQC53Gl8pI7aDg9t&index=32
//
// and there are more way to build with c
// see also build.zig file to import as module, extern fn.
//
// remember, need to use via build.zig, not working on just zig run src/main.zig
//
// run cli via zig run ....main/zig src/p04-core/clib/math.c -I src/p04-core/clib -lc

pub fn main() !void {
    const v = math.add(1, 2);

    u.print("{d}", .{v});
}
