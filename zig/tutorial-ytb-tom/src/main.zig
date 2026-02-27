const primitive = @import("p01-primitives/primitive.zig");
const conditions = @import("p02-controls/conditions.zig");
const listAndLoop = @import("p02-controls/list-and-loop.zig");
const errorHandle = @import("p03-errors/handle.zig");
const optional = @import("p03-errors/optional.zig");

pub fn main() !void {
    // try primitive.run();
    // try conditions.run();
    // try listAndLoop.run();
    // try errorHandle.run();
    try optional.run();
}
