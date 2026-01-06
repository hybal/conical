const std = @import("std");


const MODS = std.StaticStringMap([]const u8).initComptime(.{
    .{ "common", "src/common.zig" },
    .{ "lex", "src/lex/mod.zig" },
    .{ "parse", "src/parse/mod.zig" },
    .{ "hir", "src/hir/mod.zig" },
    .{ "mir", "src/mir/mod.zig" },
    .{ "sema", "src/sema/mod.zig" },
    .{ "backend", "src/backend/mod.zig" },
    .{ "diagnostics", "src/diag.zig" },
    .{ "driver", "src/driver/mod.zig" },
});

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const llvm_module = b.addModule("llvm", .{
        .root_source_file = b.path("src/llvm-c/mod.zig"),
        .target = target,
        .optimize = optimize
    });

    llvm_module.linkSystemLibrary("LLVM-19", .{});
    llvm_module.link_libc = true;


    const exe = b.addExecutable(.{
        .name = "conical",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const kvs = MODS.kvs;
    for (0..kvs.len) |i| {
        const key = kvs.keys[i];
        const value = kvs.values[i];
        const mod = b.addModule(key, .{
            .root_source_file = b.path(value),
            .target = target,
            .optimize = optimize,
        });
        exe.root_module.addImport(key, mod);
    }

    exe.root_module.addImport("llvm", llvm_module);
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

   // const lib_unit_tests = b.addTest(.{
   //     .root_source_file = b.path("src/root.zig"),
   //     .target = target,
   //     .optimize = optimize,
   // });

   // const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
   // test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_exe_unit_tests.step);
}
