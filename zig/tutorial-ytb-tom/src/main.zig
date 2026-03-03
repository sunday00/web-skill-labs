const primitive = @import("p01-primitives/primitive.zig");
const conditions = @import("p02-controls/conditions.zig");
const listAndLoop = @import("p02-controls/list-and-loop.zig");
const errorHandle = @import("p03-errors/handle.zig");
const optional = @import("p03-errors/optional.zig");
const allocate = @import("p04-mem/allocate.zig");
const compTime = @import("p05-comp-time/compTime.zig");
const platform = @import("p06-built/platform.zig");
const strictness = @import("p06-built/strict.zig");
const att = @import("p06-built/att.zig");
const ioInput = @import("p07-io/input.zig");

pub fn main() !void {
    // try primitive.run();
    // try conditions.run();
    // try listAndLoop.run();
    // try errorHandle.run();
    // try optional.run();
    // try allocate.run();
    // try compTime.run();
    // try platform.run();
    // try strictness.run();
    // try att.run();
    try ioInput.run();
}
