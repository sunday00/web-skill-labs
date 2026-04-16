const std = @import("std");
const rlz = @import("raylib_zig");

pub fn build(b: *std.Build) !void {
    const emsdk = rlz.emsdk;

    const target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .emscripten,
    });
    const optimize = b.standardOptimizeOption(.{});

    const raylib_zig_dep = b.dependency("raylib_zig", .{
        .target = target,
        .optimize = optimize,
    });

    const raylib = raylib_zig_dep.module("raylib");
    const raygui = raylib_zig_dep.module("raygui");
    const raylib_artifact = raylib_zig_dep.artifact("raylib");

    const wasm = b.addLibrary(.{
        .name = "practice_raylib",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{
                    .name = "practice_raylib",
                    .module = b.addModule("practice_raylib", .{
                        .root_source_file = b.path("src/root.zig"),
                        .target = target,
                    }),
                },
            },
        }),
    });

    wasm.root_module.linkLibrary(raylib_artifact);
    wasm.root_module.addImport("raylib", raylib);
    wasm.root_module.addImport("raygui", raygui);

    wasm.root_module.addCSourceFile(.{
        .file = b.path("src/transmission/lib.c"),
        .flags = &[_][]const u8{"-std=c99"},
    });

    const install_dir: std.Build.InstallDir = .{ .custom = "web" };
    var emcc_flags = emsdk.emccDefaultFlags(b.allocator, .{ .optimize = optimize });

    try emcc_flags.put("--shell-file", {});
    try emcc_flags.put(b.path("src/shell.html").getPath(b), {});

    try emcc_flags.put("-sEXPORTED_FUNCTIONS=['_main', '_callJs']", {});
    try emcc_flags.put("-sEXPORTED_RUNTIME_METHODS=['HEAPU8', '_callJs']", {});

    const emcc_settings = emsdk.emccDefaultSettings(b.allocator, .{ .optimize = optimize });
    const emcc_step = emsdk.emccStep(b, raylib_artifact, wasm, .{
        .optimize = optimize,
        .flags = emcc_flags,
        .settings = emcc_settings,
        .install_dir = install_dir,
    });

    b.getInstallStep().dependOn(emcc_step);

    const html_filename = try std.fmt.allocPrint(b.allocator, "{s}.html", .{wasm.name});
    const emrun_step = emsdk.emrunStep(
        b,
        b.getInstallPath(install_dir, html_filename),
        &.{},
    );

    const run_step = b.step("run", "Run the app");

    emrun_step.dependOn(emcc_step);
    run_step.dependOn(emrun_step);
}
