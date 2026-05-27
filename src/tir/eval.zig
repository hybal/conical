const std = @import("std");
const common = @import("common");
const types = @import("types").Type;
const set = @import("types").set;
const tir = @import("./Tir.zig");
const Tir = tir.Tir;

gpa: std.mem.Allocator,
compalloc: std.mem.Allocator,
tri_data: *const Tir,
env: std.AutoHashMap(tir.ResultLoc, CValue),

pub fn init(gpa: std.mem.Allocator, tir_data: *const Tir) @This() {
    const self = @This() {
        .gpa = gpa,
        .compalloc = std.heap.page_allocator, //should likely be arena
        .tir = tir_data,
        .env = .init(gpa),
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

