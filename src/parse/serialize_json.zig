const std = @import("std");
const ast = @import("Ast.zig");
const lex = @import("lex");
const common = @import("common");
const Ast = ast.Ast;

const Value = std.json.Value;

fn serialize_node(ast: *Ast, nodeid: ast.AstNodeId, reader: std.Io.Reader, builder: *std.json.) !Value {
    const kind, const node = ast.get(nodeid);
    var value = undefined;

    switch (kind) {
        .module_decl => {

        }
    }
}

