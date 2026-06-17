const std = @import("std");
const common = @import("common");
const hir = @import("hir");
const types = @import("types");
const eval = @import("./eval.zig");

const InternId = common.intern.InternId;

pub const Int = common.numbers.Int;

pub const Float = common.numbers.Float;

const SymbolId = usize;

/// Poinsts to a yet-to-be constructed initialization expression.
/// May be moved to a alloc/set pattern in the future
pub const InitRef = u32;
/// Points to constructed function arguments
pub const FnArgRef = u32;

/// Points to a constant in this blocks bss
pub const ConstantRef = u32;

/// Points to an earlier computation in this block
/// Must always point to a computation _before_ the instruction using it
pub const ResultLoc = u32;

/// Refers to a function argument, not necessarily the same order as declared,
///  nor does it refer to how arguments are passed in emitted code.
pub const ArgIndex = u16;

/// Refers to a basic block
pub const BlockId = u32;

pub const InstrKind = enum(u16) {
    // Boolean constant, inline since its only 1 byte
    bool,
    // Constant
    constant,
    // Set union
    @"union",
    // Set cartesian product
    product,
    // Set difference
    difference,
    // Set intersection
    intersection,
    range,
    // Set membership
    membership,
    // Set subset-or-equal
    subeq,
    // Set superset-or-equal
    supeq,
    // Set subset
    sub,
    // Set superset
    sup,
    // Type check, checks a inferred type against a computed one,
    //  used to model explicit type annotations.
    // Will always be executed, all operands are required to be comptime expressions.
    tychk,
    // Logical and
    land,
    // Logical or
    lor,
    // Logical not
    lnot,
    // Get an argument passed to this function, based on pre-assigned index
    arg_get,
    // Call a function with the previously set arguments
    call,
     // Tries to evaluate a block, in-which case gets replaced with the comptime result, otherwise 
    eval,
    // "evaluate" a block at runtime. Essentially a basic jump to a block, but results in the yielded value from the block.
    evalr,
    // Branch, jumps to 'then' if the passed value is true, else jumps to 'else'
    br,
    // Makes a new slot, essentially equivalent to allocating something on the stack
    //  used for anything that isn't just temporaries/constants essentially alloca
    make_slot,
    // Access a named set value
    access,
    // Load a value from a slot
    load,
    // Store a value to a slot
    store,
    // Load a value from a symbol
    load_symbol,
    // Store a value to a symbol
    store_symbol,
    // Load from memory location
    load_ptr,
    // Store to memory location
    store_ptr,
    // Returns the memory location of a given slot
    slot_ptr,
    // Yields a value from a block
    //  acts as a general-purpose "return-from-block" instruction,
    //  takes the place of 'return' in functions
    yield,
    @"return",
    // Construct an initializer, used to make sure that ordering is correct.
    init,
    _,
};

/// A single Tir instruction
pub const Instr = union(InstrKind) {
    @"bool": bool,
    constant: ConstantRef,
    @"union": struct { left: ResultLoc, right: ResultLoc },
    product: struct { left: ResultLoc, right: ResultLoc },
    difference: struct { left: ResultLoc, right: ResultLoc },
    intersection: struct { left: ResultLoc, right: ResultLoc },
    range: struct { left: ResultLoc, right: ResultLoc, lefti: bool, righti: bool },
    membership: struct { left: ResultLoc, right: ResultLoc },
    subeq: struct {left: ResultLoc, right: ResultLoc, },
    supeq: struct {left: ResultLoc, right: ResultLoc, },
    sub: struct {left: ResultLoc, right: ResultLoc},
    sup: struct {left: ResultLoc, right: ResultLoc},
    tychk: struct {expected: ResultLoc, expr: ResultLoc},
    land: struct { left: ResultLoc, right: ResultLoc },
    lor: struct { left: ResultLoc, right: ResultLoc },
    lnot: ResultLoc,
    arg_get: ArgIndex,
    call: struct { func: ResultLoc, args: FnArgRef },
    eval: BlockId,
    evalr: BlockId,
    br: struct { cond: ResultLoc, then: BlockId, @"else": BlockId },
    make_slot: ResultLoc,
    access: struct { expr: ResultLoc, symbol: InternId },
    load: ResultLoc,
    store: struct { ResultLoc, ResultLoc },
    load_symbol: SymbolId,
    store_symbol: struct { sym: SymbolId, value: ResultLoc },
    load_ptr: ResultLoc,
    store_ptr: ResultLoc,
    slot_ptr: ResultLoc,
    yield: ?ResultLoc,
    @"return": ResultLoc,
    init: InitRef,
};

comptime {
    std.debug.assert(@sizeOf(Instr) <= 24);
}


pub const FnArg = struct {
    id: ArgIndex,
    arg: ResultLoc,
};
pub const FnArgs = struct {
    args: []FnArg,
};

pub const Range = struct {
    start: usize,
    end: usize,
};


pub const BasicBlock = struct {
    // The range in the instruction list
    instructions: Range,
    span: common.Span,
    // Arguments to this block, equivalent to a phi function in SSA
    // NOTE: may be removed
    args: []ResultLoc,
};


pub const Initialization = struct {
    ty: ?ResultLoc,
    fields: []Field,
};

pub const Field = struct {
    label: ?InternId,
    value: ResultLoc,
};

pub const Constant = union(enum) {
    int: Int,
    float: Float,
    symbol: InternId,
};



pub const Function = struct {
    block: BasicBlock,
    params: []ResultLoc,
    generics: []?ResultLoc,
    ret: ResultLoc,
    sym: Symbol,
    visibility: Visibility,
    linkage: Linkage,
    is_pure: bool,
    is_comptime: bool,
    is_inline: bool,
    span: common.Span,
};

pub const Visibility = enum {
    @"pub",
    private,
};

pub const Linkage = enum {
    @"extern",
    @"export",
    internal,
};

pub const Binding = struct {
    sym: Symbol,
    expr: ResultLoc, //Should point to the **last** expression from the initialization
    mod: BindingMod,
    visibility: Visibility,
    linkage: Linkage,
    span: common.Span,
};

pub const BindingMod = enum {
    mut,
    move,
    alias,
};

/// Result of either a compile-time computation or a runtime computation
pub const Result = union(enum) {
    value: eval.CValue,
    instr: usize,
};

/// Map of the result types of every instruction/expression.
pub const TypeMap = std.AutoHashMap(ResultLoc, types.Type);

/// Map of ResultLoc -> Result where Result is either a computed value or a runtime instruction.
pub const ResultLocMap = std.AutoHashMap(ResultLoc, Result);

pub const SymbolType = union(enum) {
    expr: ResultLoc,
    block: BlockId,
    function: FunctionId,
    binding: BindingId,
};

pub const FunctionId = u32;
pub const BindingId = u32;

pub const Symbol = struct {
    id: common.intern.InternId,
    expr: SymbolType,
};

pub const SymbolTable = std.AutoHashMap(SymbolId, Symbol);

pub const SpanTable = std.AutoHashMap(ResultLoc, common.Span);

pub const Tir = struct {
    functions: []Function,
    bindings: []Binding,
    instructions: []Instr,
    blocks: []BasicBlock,
    //Maybe move these to a single 'extra' array in the future?
    constants: []Constant,
    initializations: []Initialization,
    fn_args: []FnArgs,
    sets: []types.Type,

    spans: SpanTable,
};

pub const TirBuilder = struct {
    functions: std.ArrayList(Function),
    bindings: std.ArrayList(Binding),
    instructions: std.ArrayList(Instr),
    blocks: std.ArrayList(BasicBlock),
    allocator: std.mem.Allocator,

    symtab: SymbolTable,

    pub const BlockBuilder = struct {
        self: *TirBuilder,
        constants: std.ArrayList(Constant),
        initializations: std.ArrayList(Initialization),
        fn_args: std.ArrayList(FnArgs),
        sets: std.ArrayList(types.Type),
        resultmap: ResultLocMap,
        start: usize,
        end: usize,
        
        pub fn init(self: *TirBuilder, start: usize) @This() {
            const blk = BlockBuilder {
                .self = self,
                .constants = .empty,
                .initializations = .empty,
                .fn_args = .empty,
                .sets = .empty,
                .resultmap = .empty,
                .start = start,
                .end = start + 1,
            };
            return blk;
        }

        pub fn add_instr(self: *@This(), instr: Instr) !ResultLoc {
            const instr_index = try self.self.append(&self.self.instructions, instr);
            self.end = instr_index;
            const resultloc = try self.self.resultmap.put(.{ .instr = instr });
            return resultloc;
        }
    };

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .functions = .empty,
            .bindings = .empty,
            .instructions = .empty,
            .resultmap = .init(allocator),
            .allocator = allocator,
            .symtab = .init(allocator),
        };
    }

    fn append(self: *@This(), array: anytype, value: anytype) !usize {
        try array.append(self.allocator, value);
        return array.items.len - 1;
    }

    pub fn add_function(self: *@This(), function: Function) !void {
        try self.functions.append(self.allocator, function);
    }

    pub fn add_binding(self: *@This(), binding: Binding) !void {
        try self.bindings.append(self.allocator, binding);
    }

    pub fn add_block(self: *@This(), block: BasicBlock) !BlockId {
        return try self.append(&self.blocks, block);
    }

    pub fn get_block(self: *@This(), block: BlockId) BasicBlock {
        return self.blocks.items[block];
    }

    pub fn get_result(self: *@This(), block: BlockId, resultloc: ResultLoc) Result {
        return self.get_block(block).resultmap.items[resultloc];
    }

    pub fn get_instr(self: *@This(), block: BlockId, resultloc: ResultLoc) ?Instr {
        const result = self.get_result(block, resultloc);
        if (result == .instr) return self.get_block(block).instructions[result.instr];
        return null;
    }


    pub fn build(self: *@This()) !Tir {
        return Tir{
            .bindings = try self.bindings.toOwnedSlice(self.allocator),
            .functions = try self.functions.toOwnedSlice(self.allocator),
            .blocks = try self.blocks.toOwnedSlice(self.allocator),
            .instructions = try self.instructions.toOwnedSlice(self.allocator),
            .resultmap = self.resultmap,
        };
    }
};
