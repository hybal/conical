const std = @import("std");
const ModInf = struct {
    path: []const u8,
    deps: []const []const u8,
};

const MODS = std.StaticStringMap(ModInf).initComptime(.{
    .{ "common", ModInf { .path = "src/common.zig", .deps = &.{ "diagnostics"} } },
    .{ "lex", ModInf { .path  = "src/lex/mod.zig", .deps = &.{"common"} } },
    .{ "parse", ModInf { .path  = "src/parse/mod.zig", .deps = &.{"common", "lex", "diagnostics"} } },
    .{ "diagnostics", ModInf { .path  = "src/diagnostics/mod.zig", .deps = &.{"common"} } },
    .{ "hir", ModInf { .path  = "src/hir/mod.zig", .deps = &.{"common", "diagnostics"} } },
    .{ "mir", ModInf { .path  = "src/mir/mod.zig", .deps = &.{"common", "diagnostics"} } },
    .{ "sema", ModInf { .path  = "src/sema/mod.zig", .deps = &.{"common", "diagnostics"} } },
    .{ "backend", ModInf { .path  = "src/backend/mod.zig", .deps = &.{"common", "diagnostics"} } },
    .{ "bindings", ModInf { .path = "src/bindings/mod.zig", .deps = &.{} }},
    .{ "driver", ModInf { .path  = "src/driver/mod.zig", .deps = &.{"common", "diagnostics"} } },
});

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "conical",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    const exe_unit_tests = b.addTest(.{
        .target = target,
        .optimize = optimize,
        .root_source_file = b.path("src/test_main.zig"),
    });

    var modules = std.StringHashMap(*std.Build.Module).init(b.allocator);
    const kvs = MODS.kvs;
    for (0..kvs.len) |i| {
        const name = kvs.keys[i];
        const info = kvs.values[i];

        const mod = b.addModule(name, .{
            .root_source_file = b.path(info.path),
            .target = target,
            .optimize = optimize,
        });

        modules.put(name, mod) catch @panic("Ran out of memory");
        exe.root_module.addImport(name, mod);
        //exe_unit_tests.root_module.addImport(name, mod);
    }
    for (0..kvs.len) |i| {
        const name = kvs.keys[i];
        const info = kvs.values[i];
        const mod = modules.get(name).?;

        for (info.deps) |dep_name| {
            const dep_mod = modules.get(dep_name)
                orelse @panic("unkown module dependency");
            mod.addImport(dep_name, dep_mod);
        }
    }
    modules.getPtr("bindings").?.*.link_libc = true;

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

    
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    // test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_exe_unit_tests.step);
}
