const std = @import("std");
const Hir = @import("Hir.zig");
const types = @import("types.zig");
const Ast = @import("Ast.zig");


context: *types.Context,
allocator: std.mem.Allocator,
current_escape: Hir.EscapeInfo = .Local,
// HIR id to put cleanup before -> HIR id's that need destructing (in correct order)
escaped_ids: std.AutoHashMap(Hir.HirId, std.ArrayList(Hir.HirId)),
hir_info_table: *Hir.HirInfoTable,


pub fn resolve(self: *@This(), hir: []Hir.Hir) !void {
    for (hir) |node| {
        try self.resolve_inner(node, .Local);
    }
}

fn resolve_inner(self: *@This(), hir: Hir.Hir, escape: Hir.EscapeInfo) !void {
    const hir_info = self.hir_info_table.get(hir).?;
    switch (hir.node) {
        .top_level => {
            switch (hir.node.top_level) {
                .type_decl
            }
        },
        .inline_expr => {
            switch (hir.node.top_level) {

            }
        },
    }
}



