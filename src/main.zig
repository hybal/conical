const std = @import("std");
const diag = @import("diagnostics");
const common = @import("common");
const lex = @import("lex");
const parse = @import("parse");
//const hir = @import("hir");
//const mir = @import("mir");
//const sema = @import("sema");
//const backend = @import("backend");
//const bindings = @import("bindings");
//const driver = @import("driver");



pub fn main(init: std.process.Init) !u8 {
    const buffer = 
        \\mod a;
        \\fn main() {
        \\  let a = 1;
        \\ }
    ;
    var reader = std.Io.Reader.fixed(buffer);
    var ctx = common.Context.init(std.heap.page_allocator);
    const file = try ctx.file_store.put(.{.buffer = buffer});
    var parser = parse.init(&ctx, &reader, file, std.heap.page_allocator);
    _ = try parser.parse();
    var buff: [64]u8 = undefined;
    const stderr = try init.io.lockStderr(&buff, null);
    try ctx.session.emit(&ctx, init.io, &stderr.file_writer.interface);
    init.io.unlockStderr();
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



