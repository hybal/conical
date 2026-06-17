const std = @import("std");
const common = @import("common");
const types = @import("types").Type;
const set = @import("types").set;
const tir = @import("./Tir.zig");
const Tir = tir.Tir;

gpa: std.mem.Allocator,
compalloc: std.mem.Allocator,
builder: *const tir.TirBuilder,
env: std.AutoHashMap(tir.ResultLoc, CValue),
context: *common.Context,

pub fn init(gpa: std.mem.Allocator, context: *common.Context, builder: *const tir.TirBuilder) @This() {
    const self = @This() {
        .gpa = gpa,
        .compalloc = std.heap.page_allocator, //should likely be arena
        .builder = builder,
        .env = .init(gpa),
        .context = context,
    };
    return self;
}

pub const CValue = union(enum) {
    int: tir.Int,
    float: tir.Float,
    symbol: common.intern.InternId,
    @"bool": bool,
    @"type": types.Type,
    initializer: tir.Initialization,
    array: []CValue,
    slot: u32,
    data: []u8, //will probably be removed
};


pub fn try_eval_block(self: *@This(), blockid: tir.BlockId) ?CValue {
    _ = self;
    _ = blockid;
}

pub fn try_eval(self: *@This(), blockid: tir.BlockId, res: tir.ResultLoc) ?CValue {
    const instr: tir.Instr = self.tri_data.blocks[blockid].instructions[res]; 
    const block = self.tri_data.blocks[blockid];
    switch (instr) {
        .constant => |cref| {
            return switch (block.constants[cref]) {
                .float => |f| CValue {.float = f},
                .int => |i| CValue {.int = i},
                .symbol => |s| CValue {.symbol = s}
            };
        },
        .@"bool" => |b| {
            return CValue { .@"bool" = b};
        },
    }
}

