const std = @import("std");

pub fn build(b: *std.Build) void {
    const target_query = std.Target.Query{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    };

    const target = b.resolveTargetQuery(target_query);

    const lib = b.addExecutable(.{
        .name = "wasmstr",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = .ReleaseSmall,
        }),
    });

    lib.root_module.export_symbol_names = &.{ "alloc", "free", "add", "sub", "zlog" };

    lib.entry = .disabled;

    b.installArtifact(lib);
}
