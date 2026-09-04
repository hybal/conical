const std = @import("std");
const diag = @import("diagnostics");
const common = @import("common");
const lex = @import("lex");
const parse = @import("parse");
//const hir = @import("hir");
//const tir = @import("tir");
//const sets = @import("types").set;
//const mir = @import("mir");
//const sema = @import("sema");
//const backend = @import("backend");
//const bindings = @import("bindings");
//const driver = @import("driver");



pub fn main(init: std.process.Init) !u8 {
    const buffer_old = 
        \\mod type ide: ide | main: main;
        \\let A = type rel i32 | 1;
        \\fn ide(a: i32, b: i32) => {
        \\  1 + 2
        \\}
        \\fn main() => {
        \\  let mut a = 2;
        \\ }
    ;
    _ = buffer_old;
    var alloc = std.heap.DebugAllocator(.{ 
    }).init;
    const gpa = alloc.allocator();
    var ctx = common.Context.init(gpa);

    var args_iter = init.minimal.args.iterate();
    _ = args_iter.skip();
    const path = args_iter.next();
    if (path == null) {
        std.debug.print("Error: requires file path\n", .{});
        return 1;
    }
    const pathf = try common.path.Path.create(path.?, gpa);
    const file = try ctx.file_store.put(.{ .file = pathf });
    const buffer = try std.Io.Dir.cwd().readFileAlloc(init.io, path.?, gpa, .unlimited);
    //_ = hir;
    const buff: []u8 = try gpa.alloc(u8, 10*1024);
    var parser = try parse.init(&ctx, buffer, file, gpa);
    _ = parser.parse() catch |e| {
        return e;
    };
 //   var hir_ctx = hir.lower.init(gpa, &ctx, file, buffer, &ast);
 //   _ = try hir_ctx.lower();
    const stderr = std.debug.lockStderr(buff);
    try ctx.session.emit(&ctx, init.io, &stderr.file_writer.interface);
    std.debug.unlockStderr();
    defer _ = alloc.deinitWithoutLeakChecks();
    
    return 0;
}

test {
    //_ = @import("tests.zig");
    //_ = @import("parse");
    //try std.testing.expect(false);
    //std.testing.refAllDeclsRecursive(hir);
    //std.testing.refAllDeclsRecursive(mir);
    //std.testing.refAllDeclsRecursive(sema);
    //std.testing.refAllDeclsRecursive(bindings);
    //std.testing.refAllDeclsRecursive(driver);
}



