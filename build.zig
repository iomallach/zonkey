const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    // This creates a "module", which represents a collection of source files alongside
    // some compilation options, such as optimization mode and linked system libraries.
    // Every executable or library we compile will be based on one or more modules.
    const zonkey_module = b.addModule("zonkey", .{
        // `root_source_file` is the Zig "entry point" of the module. If a module
        // only contains e.g. external object files, you can make this `null`.
        // In this case the main source file is merely a path, however, in more
        // complicated build scripts, this could be a generated file.
        .root_source_file = b.path("src/zonkey.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "zonkey",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.addSystemIncludePath(.{ .cwd_relative = "/opt/homebrew/Cellar/llvm/19.1.7_1/include" });
    exe.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/Cellar/llvm/19.1.7_1/lib" });
    exe.linkLibC();
    exe.linkSystemLibrary("LLVM");
    b.installArtifact(exe);

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const tests = b.addTest(.{ .root_module = b.createModule(.{ .root_source_file = b.path("tests/lexer.zig"), .target = target, .optimize = optimize, .imports = &.{
        .{ .name = "zonkey", .module = zonkey_module },
    } }) });
    tests.root_module.addImport("zonkey", zonkey_module);

    const tests_step = b.step("test", "Run all tests");
    const run_tests = b.addRunArtifact(tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    tests_step.dependOn(&run_tests.step);
}
