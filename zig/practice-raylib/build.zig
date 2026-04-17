const std = @import("std");
const rlz = @import("raylib_zig");

pub fn build(b: *std.Build) void {
    // ==============================SET ZERO==============================
    // ==============================SET ZERO==============================

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.addModule("practice_raylib", .{
        .root_source_file = b.path("src/root.zig"),

        .target = target,
    });

    // ==============================MAIN MODULE==============================
    // ==============================MAIN MODULE==============================

    const exe = b.addExecutable(.{
        .name = "practice_raylib",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "practice_raylib", .module = mod },
            },
        }),
    });

    // ==============================C MODULE==============================
    // ==============================C MODULE==============================

    exe.root_module.addCSourceFile(.{
        .file = b.path("src/transmission/lib.empty.c"),
        .flags = &[_][]const u8{"-std=c99"},
    });

    // ==============================PACKAGE==============================
    // ==============================PACKAGE==============================

    const raylib_zig_dep = b.dependency("raylib_zig", .{
        .target = target,
        .optimize = optimize,
    });

    const raylib = raylib_zig_dep.module("raylib");
    const raygui = raylib_zig_dep.module("raygui");
    const raylib_artifact = raylib_zig_dep.artifact("raylib");

    raylib_artifact.root_module.addCMacro("SUPPORT_FILEFORMAT_MP3", "1");

    exe.root_module.linkLibrary(raylib_artifact);
    exe.root_module.addImport("raylib", raylib);
    exe.root_module.addImport("raygui", raygui);

    // ==============================RUNTIME==============================
    // ==============================RUNTIME==============================
    b.installArtifact(exe);

    const run_step = b.step("run", "Run the app");
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // ==============================TESTS==============================
    // ==============================TESTS==============================
    const mod_tests = b.addTest(.{
        .root_module = mod,
    });
    const run_mod_tests = b.addRunArtifact(mod_tests);
    const exe_tests = b.addTest(.{
        .root_module = exe.root_module,
    });
    const run_exe_tests = b.addRunArtifact(exe_tests);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_mod_tests.step);
    test_step.dependOn(&run_exe_tests.step);
}
