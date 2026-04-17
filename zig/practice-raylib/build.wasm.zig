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
    // const preload_dir: emsdk.EmccFilePath = .{
    //     .src_path = "src/assets",
    //     // .virtual_path: ?[]const u8 = null,
    // };

    const emcc_flags = emsdk.emccDefaultFlags(b.allocator, .{
        .optimize = optimize,
    });

    // try emcc_flags.put("--preload-file", {});
    // try emcc_flags.put(b.path("assets").getPath(b), {});

    var emcc_settings = emsdk.emccDefaultSettings(b.allocator, .{ .optimize = optimize });
    try emcc_settings.put("EXIT_RUNTIME", "1");
    try emcc_settings.put("STACK_SIZE", "5MB");
    try emcc_settings.put("INITIAL_MEMORY", "64MB");
    // try emcc_settings.put("ALLOW_MEMORY_GROWTH", "1");

    const emcc_step = emsdk.emccStep(b, raylib_artifact, wasm, .{
        .optimize = optimize,
        .flags = emcc_flags,
        .settings = emcc_settings,
        .install_dir = install_dir,
        .shell_file_path = b.path("src/shell.html"),
        .preload_paths = &.{.{
            .src_path = "assets",
        }},
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
