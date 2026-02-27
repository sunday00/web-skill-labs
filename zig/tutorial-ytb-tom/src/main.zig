const primitive = @import("p01-primitives/primitive.zig");
const conditions = @import("p02-controls/conditions.zig");
const listAndLoop = @import("p02-controls/list-and-loop.zig");

pub fn main() !void {
    // try primitive.run();
    // try conditions.run();
    try listAndLoop.run();
}
