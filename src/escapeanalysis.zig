const std = @import("std");
const Hir = @import("Hir.zig");
const types = @import("types.zig");
const Ast = @import("Ast.zig");


context: *types.Context,
allocator: std.mem.Allocator,
current_escape: Hir.EscapeInfo = .Local,
// HIR id to put cleanup before -> HIR id's that need destructing (in correct order)
// This would likely go before return statements / at the end of scopes
destroy_ids: std.AutoHashMap(Hir.HirId, std.ArrayList(Hir.HirId)),
escaped_ids: std.AutoHashMap(Hir.HirId, void),
hir_info_table: *Hir.HirInfoTable,
current_scope: u64,


pub fn resolve(self: *@This(), hir: []Hir.Hir) !void {
    for (hir) |node| {
        try self.resolve_inner(node, .Local);
    }
}

fn resolve_inner(self: *@This(), hir: Hir.Hir, propogate: Hir.EscapeInfo) !void {
    const hir_info = self.hir_info_table.get(hir).?;
    switch (hir.node) {
        .top_level => {
            switch (hir.node.top_level) {
                .return_stmt => |ret| {
                }
            }
        },
        .inline_expr => {
            switch (hir.node.inline_expr) {
               .terminal => |term| {
                   switch (term.*) {
                       .path => |pth| {
                           const symid = self.context.
                               sym_tab.items[self.current_scope].symbol_map.get(pth).?;
                       }
                   }
               },
            }
        },
    }
}



