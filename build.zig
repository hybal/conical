const std = @import("std");
const ModInf = struct {
    path: []const u8,
    deps: []const []const u8,
};

const MODS = std.StaticStringMap(ModInf).initComptime(.{
    .{ "common", ModInf { .path = "src/common/mod.zig", .deps = &.{ "diagnostics"} } },
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
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    var modules = std.StringHashMap(*std.Build.Module).init(b.allocator);
    const kvs = MODS.kvs;
    // Create overall modules
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
    }

    // Wire dependencies between modules
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

    // Add tests
    const test_step = b.step("test", "Run unit tests");
    var mod_it = modules.iterator();
    while (mod_it.next()) |entry| {
        const tst = b.addTest(.{
            .root_module = entry.value_ptr.*,
        });
        const test_run = b.addRunArtifact(tst);
        test_step.dependOn(&test_run.step);
    }
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


}
