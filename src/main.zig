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


pub fn main() !u8 {
    return 0;
}

test "main" {
    std.testing.refAllDecls(diag);
    std.testing.refAllDecls(common);
    std.testing.refAllDeclsRecursive(lex);
    std.testing.refAllDeclsRecursive(parse);
    //try std.testing.expect(false);
    //std.testing.refAllDeclsRecursive(hir);
    //std.testing.refAllDeclsRecursive(mir);
    //std.testing.refAllDeclsRecursive(sema);
    //std.testing.refAllDeclsRecursive(bindings);
    //std.testing.refAllDeclsRecursive(driver);
}

