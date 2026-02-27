const primitive = @import("p01-primitives/primitive.zig");
const control = @import("p02-controls/control.zig");

pub fn main() !void {
    // try primitive.run();
    try control.run();
}
