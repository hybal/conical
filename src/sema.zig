const std = @import("std");
const Hir = @import("Hir.zig");
const Ast = @import("Ast.zig");
const types = @import("types.zig");
const mem = @import("mem.zig");
const diag = @import("diag.zig");


symtree: std.ArrayList(types.SymbolTable),
type_map: types.TypeTbl,
hir_table: Hir.HirInfoTable,
source: []const u8,
gpa: std.mem.Allocator,
expected_type: ?Ast.TypeId = null,
function_return_type: ?Ast.TypeId = null,
session: *diag.Session,
in_func: bool = false,
in_loop: bool = false,
in_assignment: bool = false,
in_global_scope: bool = true,
current_scope: usize = 0,

pub fn init_context(symtree: std.ArrayList(types.SymbolTable),
        hir_table: Hir.HirInfoTable,
        type_map: types.TypeTbl,
        source: []const u8,
        gpa: std.mem.Allocator,
        session: *diag.Session,
        ) @This() {
    return .{
        .symtree = symtree,
        .type_map = type_map,
        .source = source,
        .gpa = gpa,
        .session = session,
        .hir_table = hir_table
    };
}

fn type_check_expect(self: *@This(), tree: Hir.Hir, ty: Ast.TypeId) !void {
    const previous_expected_type = self.expected_type;
    self.expected_type = ty;
    try self.type_check(tree);
    self.expected_type = previous_expected_type;
}

pub fn analyze(self: *@This(), trees: []Hir.Hir) !void {
    for (trees) |*tree| {
        try self.type_check(tree);
    }
}

fn type_check(self: *@This(), tree: *Hir.Hir) !void {
    const hir_info = self.hir_table.get(tree.id).?;
    switch (tree.node) {
        .top_level => |node| {
            switch (node) {
                .func => |func| {
                    var function_type: Ast.TypeId = undefined;
                    if (hir_info.ty) |tyid| {
                        function_type = tyid;
                    } else {
                        var arg_types: std.ArrayList(Ast.TypeId) = .init(self.gpa);
                        for (func.parameters) |param| {
                            try arg_types.append(param.ty);
                        }
                        const ty: Ast.Type = .{
                            .base_type = .{
                                .func = .{
                                    .args = try arg_types.toOwnedSlice(),
                                    .ret = func.return_type,
                                }
                            },
                            .modifiers = null,
                        };
                        function_type = ty.hash();
                    }
                },
                else => unreachable,
            }
        },
        .inline_expr => |node| {
            switch (node) {
                else => unreachable,
            }
        },

    }
}

