const std = @import("std");
const codegen = @import("lower_tmp.zig");
const llvm = @import("llvm");
const lex = @import("lexer.zig");
const ast = @import("Ast.zig");
const parse = @import("parser.zig");
const sema = @import("sema.zig");
const types = @import("types.zig");
const diag = @import("diag.zig");
const lower_hir = @import("lower.zig");
const emit = @import("emit.zig");
//const escapeanalysis = @import("escapeanalysis.zig");


pub fn main() !u8 {
    const page_allocator = std.heap.page_allocator;
    var arena_alloc = std.heap.ArenaAllocator.init(page_allocator);
    const gpa = arena_alloc.allocator();
    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    if (args.len < 2) {
        return error.NotEnoughCmdlineArgs;
    }

    const file = try std.fs.cwd().openFile(args[1], .{});
    defer file.close();

    const source = try file.readToEndAlloc(gpa, std.math.maxInt(usize));

    //const type_map = try types.init_type_map(gpa); 
    var module_store = types.ModuleStore {
        .store = .init(gpa),
        .trie = .{
            .children = .init(gpa),
            .value = null,
        },
        .lock = .{}
    };
    const context = types.Context {
        .session = diag.Session.init(gpa, source),
        .source = source,
        //.type_tab = type_map,
        .sym_tab = .init(gpa),
        .file_path = args[1],
        .module = null,
        .module_store = &module_store,
    };
    _ = context;
    _ = ast.Ast;
    _ = ast.Ast.get;
    _ = ast.AstBuilder;
    _ = ast.AstBuilder.add_node;
    _ = ast.AstBuilder.build;
    _ = ast.AstBuilder.init;
   //var parser = parse.init(&context, gpa);
   // const trees = parser.parse() catch |err| {
   //     try context.session.flush(std.io.getStdErr().writer());
   //     return err;
   // };
   // var hir_context = try lower_hir.init(&context, gpa);
   // const hir = hir_context.lower(trees) catch |err| {
   //     try context.session.flush(std.io.getStdErr().writer());
   //     return err;
   // };

   // var sema_context = sema.init(&context, hir_context.hir_table, gpa);
   // sema.analyze(&sema_context, hir) catch |err| {
   //     try context.session.flush(std.io.getStdErr().writer());
   //     return err;
   // };
   // try context.session.flush(std.io.getStdErr().writer());

   // const triple = llvm.TargetMachine.LLVMGetDefaultTargetTriple();

   // var emit_context = try emit.init(
   //     &context, 
   //     &hir_context.hir_table, 
   //     triple, 
   //     &hir_context.escaped_ids,
   //     gpa);
   // try emit_context.emit(hir);


    return 0;
}
