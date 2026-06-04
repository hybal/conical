const std = @import("std");
const diag = @import("diagnostics");
const common = @import("common");
const lex = @import("lex");
const parse = @import("parse");
const hir = @import("hir");
const tir = @import("tir");
const sets = @import("types").set;
//const mir = @import("mir");
//const sema = @import("sema");
//const backend = @import("backend");
//const bindings = @import("bindings");
//const driver = @import("driver");



pub fn main(init: std.process.Init) !u8 {
    const buffer = 
        \\mod a;
        \\let A = type i32 | 1;
        \\fn ide(a, b): (i32, i32) -> i32 { 1}
        \\fn main() {
        \\  let mut a = 2;
        \\ }
    ;
    var alloc = std.heap.DebugAllocator(.{ 
        .never_unmap = true,
        .retain_metadata = true,
    }).init;
    const gpa = alloc.allocator();
    var ctx = common.Context.init(gpa);
    const file = try ctx.file_store.put(.{.buffer = buffer});
    _ = hir;
    var parser = try parse.init(&ctx, buffer, file, gpa);
    _ = try parser.parse();
    //var hir_ctx = hir.lower.init(gpa, &ctx, file, buffer, &ast);
    //_ = try hir_ctx.lower();
    var buff: [64]u8 = undefined;
    const stderr = try init.io.lockStderr(&buff, null);
    try ctx.session.emit(&ctx, init.io, &stderr.file_writer.interface);
    _ = alloc.deinitWithoutLeakChecks();
    std.debug.print("DEBUG: {}\n", .{@sizeOf(tir.Tir.Instr)});
    _ = tir.eval.try_eval;
    _ = tir.eval.try_eval_block;
    return 0;
}

test {
    _ = @import("tests.zig");
    _ = @import("parse");
    //try std.testing.expect(false);
    //std.testing.refAllDeclsRecursive(hir);
    //std.testing.refAllDeclsRecursive(mir);
    //std.testing.refAllDeclsRecursive(sema);
    //std.testing.refAllDeclsRecursive(bindings);
    //std.testing.refAllDeclsRecursive(driver);
}



