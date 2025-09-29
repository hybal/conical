const std = @import("std");
const Hir = @import("Hir.zig");
const Ast = @import("Ast.zig");
const types = @import("types.zig");
const diag = @import("diag.zig");
const llvm = @import("llvm");

allocator: std.mem.Allocator,

comp_unit: types.CompUnit,
session: *diag.Session,

llvm_context: struct {
    builder: llvm.Core.LLVMValueRef,
    context: llvm.Core.LLVMContextRef,
    target_machine: llvm.TargetMachine.LLVMTargetMachineRef,
    data_layout: llvm.TargetMachine.LLVMTargetDataRef,
    module: llvm.Core.LLVMModuleRef,
    current_function: ?llvm.Core.LLVMValueRef = null,
    pm: llvm.Core.LLVMPassManagerRef,
},

in_assignment: bool,
target_size: u8,
at_global: bool = true,
current_scope: usize = 0,

pub fn init(allocator: std.mem.Allocator, comp_unit: types.CompUnit, session: *diag.Session) @This() {
    return .{
        .allocator = allocator,
        .comp_unit = comp_unit,
        .session = session,
    };
}

pub fn createTypeRef(self: *@This(), ty: Ast.Type,) !llvm.Core.LLVMTypeRef {
    var base_type: llvm.Core.LLVMTypeRef = null;
    switch (ty.base_type) {
        .primitive => |prim| {
            switch (prim) {
                .I8, .U8, .Rune=>  base_type = llvm.Core.LLVMInt8TypeInContext(self.llvm_context.context),
                .I16, .U16 => base_type = llvm.Core.LLVMInt16TypeInContext(self.llvm_context.context),
                .I32, .U32=> base_type = llvm.Core.LLVMInt32TypeInContext(self.llvm_context.context),
                .I64, .U64 => base_type = llvm.Core.LLVMInt64TypeInContext(self.llvm_context.context),
                .I128, .U128 => base_type = llvm.Core.LLVMInt128TypeInContext(self.llvm_context.context),
                .ISize, .USize => {
                    const ptr_size = llvm.TargetMachine.LLVMPointerSize(self.llvm_context.data_layout) * 8;
                    base_type = llvm.Core.LLVMIntTypeInContext(self.llvm_context.context, ptr_size);
                },
                .F32 => base_type = llvm.Core.LLVMFloatTypeInContext(self.llvm_context.context),
                .F64 => base_type = llvm.Core.LLVMDoubleTypeInContext(self.llvm_context.context),
                .Bool => base_type = llvm.Core.LLVMInt1TypeInContext(self.llvm_context.context),
                .Never, .Unit => base_type = llvm.Core.LLVMVoidTypeInContext(self.llvm_context.context),
            }
        },
        .strct => |strct| {
            var iter = strct.fields.valueIterator();
            var arr = std.ArrayList(llvm.Core.LLVMTypeRef).init(self.allocator);
            while (iter.next()) |next| {
                const llvm_type = try self.createTypeRef(self.comp_unit.type_table.get(next.*).?);
                try arr.append(llvm_type);
            }
            const slc = try arr.toOwnedSlice();
            base_type = llvm.Core.LLVMStructTypeInContext(self.llvm_context.context, slc.ptr, @intCast(slc.len), 0);
        },
        .@"enum" => |enm| {
            var max_size: c_ulonglong = 0;
            var iter = enm.variants.valueIterator();
            while (iter.next()) |next| {
                if (next.*) |nxt| {
                    const nxt_ty = self.comp_unit.type_table.get(nxt).?;
                    const llvm_ty = try self.createTypeRef(nxt_ty);
                    const size = llvm.Target.LLVMABISizeOfType(self.llvm_context.target_data, @ptrCast(llvm_ty));
                    if (size > max_size) max_size = size;

                }
            }
            const storage_ty = llvm.Core.LLVMArrayType(try self.createTypeRef(Ast.Type.createPrimitive(.I8, null)), @intCast(max_size));
            const tag_ty = try self.createTypeRef(
                Ast.Type.createPrimitive(
                    Ast.PrimitiveType.from_int(@intCast(enm.variants.count()), false),
                    null
                )
            );

            if (max_size > 0) {
            const tagged_union = llvm.Core.LLVMStructTypeInContext(self.llvm_context.context,
                @constCast(&[_]llvm.Core.LLVMTypeRef{tag_ty, storage_ty}),
                2,
                0
            );
            base_type = tagged_union;
            } else {
                base_type = tag_ty;
            }
        },
        .func => |fnc| {
            var fnc_args = std.ArrayList(llvm.Core.LLVMTypeRef).init(self.allocator);
            for (fnc.args) |argid| {
                const arg = self.comp_unit.type_table.get(argid).?;
                try fnc_args.append(try self.createTypeRef(arg));
            }
            const fnc_args_slice = try fnc_args.toOwnedSlice();
            const ret_ty = try self.createTypeRef(self.comp_unit.type_table.get(fnc.ret).?);
            base_type = llvm.Core.LLVMFunctionType(ret_ty, @ptrCast(fnc_args_slice), @intCast(fnc_args_slice.len), 0);
        },
        else => unreachable,
    }
    if (ty.modifiers) |mods| {
        if (mods.len > 0) {
            var i: usize = mods.len;
            while (i > 0) : ( i -= 1 ){
                switch (mods[i - 1]) {
                    .Ref, .RefMut, .RefConst, .Ptr, .PtrMut, .PtrConst => {
                        base_type = llvm.Core.LLVMPointerType(base_type, 0);
                    },
                    .Slice => {
                        const ptr_type = llvm.Core.LLVMPointerType(base_type, 0);
                        base_type = llvm.Core.LLVMStructType(
                            @constCast(&[_]llvm.Core.LLVMTypeRef{ ptr_type, llvm.Core.LLVMIntType(self.target_size) }),
                            2,
                            0
                        );
                    },
                    else => unreachable,
                }
            }
        }
    }
    return base_type;
}

fn convert_string_to_llvm(self: *@This(), ident: types.DefId) ![*]const u8 {
    const str = self.comp_unit.symbol_table.
}

fn emit_global(self: *@This(), trees: []Hir.Hir) !void {
    for (trees) |hir| {
        const hir_info = self.comp_unit.hir_info.get(hir.id).?;
        const hir_ty = self.comp_unit.type_table.get(hir_info.ty.?).?;
        switch (hir.node) {
            .top_level => |top_level| {
                switch (top_level) {
                    .func => |func| {
                        const llvm_fnc_ty = try self.createTypeRef(hir_ty);
                    },
                }
            },
        }

    }
}
