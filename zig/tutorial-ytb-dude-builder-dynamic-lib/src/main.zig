const std = @import("std");

pub fn main() !void {
    // Get the library name via command line arg.
    var args = std.process.args();
    const bin = args.next().?; // binary name
    const lib_path = args.next() orelse {
        std.debug.print("\n\nusage: {s} <library_path>\n\n", .{bin});
        return error.MissingArg;
    };

    // Open the library.
    var lib = try std.DynLib.open(lib_path);
    defer lib.close();

    // Lookup a function.
    const funcType = *const fn (i32, i32) callconv(.c) i32;
    const calc = lib.lookup(funcType, "calc") orelse return error.NoSuchFunction;

    // Call it!
    std.debug.print("calc : {}\n", .{calc(7, 8)});
}
