const std = @import("std");
const common = @import("common");
const hir = @import("hir");
const types = @import("types");

const InternId = common.intern.InternId;
pub const Int = u128;
pub const Float = f64;
// Not actually implemented yet, but will be a reference to a global symbol
const Symbol = usize;


/// Points to a constant in this blocks bss
pub const ConstantRef = u32;


/// Points to an earlier computation in this block
/// Must always point to a computation _before_ the instruction using it
pub const ResultLoc = u32;

/// Refers to a function argument, not necessarily the same order as declared.
pub const ArgIndex = u16;

/// Refers to a basic block
pub const BlockId = u32;

pub const InstrKind = enum(u16) {
    // Boolean constant
    @"bool",
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
    // Set membership
    membership,
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
    // Unconditional block-based jump, CANNOT be used for arbitrary jumps
    jmp,
    // Conditional-true jump, jumps if the passed condition is true 
    jt,
    // Conditional-false jump, jumps if the passed condition is false
    jf,
    // Makes a new slot, essentially equivalent to allocating something on the stack
    //  used for anything that isn't just temporaries/constants essentially alloca
    make_slot,
    // Access a named set value
    access,
    // Load a value from a slot
    load,
    // Store a value to a slot
    store,
    // Load a value from an array
    load_arr,
    // Store a value to an array
    store_arr,
    // Load a value from a global symbol
    load_global,
    // Store a value to a global symbol
    store_global,
    // Yields a value from a block
    //  acts as a general-purpose "return-from-block" instruction,
    //  takes the place of 'return' in functions
    yield,
    // Tries to evaluate a block, in-which case gets replace with the comptime result,
    // otherwise is kept to represent a unevaluated block 'yield' value.
    eval,
    // Construct an initializer
    init,
    _,
};

/// A single Tir instruction
pub const Instr = union(InstrKind) {
    // Stored inline since its cheaper
    @"bool": bool,
    constant: ConstantRef,
    @"union": struct { left: ResultLoc, right: ResultLoc },
    product: struct { left: ResultLoc, right: ResultLoc },
    difference: struct { left: ResultLoc, right: ResultLoc },
    intersection: struct { left: ResultLoc, right: ResultLoc },
    membership: struct {left: ResultLoc, right: ResultLoc },
    land: struct {left: ResultLoc, right: ResultLoc},
    lor: struct {left: ResultLoc, right: ResultLoc},
    lnot: ResultLoc,
    arg_get: ArgIndex,
    call: struct {func: ResultLoc, args: FnArgRef},
    jmp: BlockId,
    jt: struct {BlockId, ResultLoc},
    jf: struct {BlockId, ResultLoc},
    make_slot: ResultLoc,
    access: struct {ResultLoc, InternId},
    load: ResultLoc,
    store: struct {ResultLoc, ResultLoc},
    load_arr: struct {ResultLoc, ResultLoc},
    store_arr: struct {ResultLoc, ResultLoc},
    load_global: Symbol,
    store_global: struct{ sym: Symbol, value: ResultLoc },
    yield: ResultLoc,
    eval: BlockId,
    init: InitRef,

};

pub const InitRef = u32;
pub const FnArgRef = u32;

pub const FnArgs = struct {
    id: ArgIndex,
    args: []ResultLoc,
};

pub const BasicBlock = struct {
    // The containing block of this block
    parent: BlockId,
    // Whether to evaluate the block during analysis
    comp_eval: bool,
    // The instructions themselves
    instructions: []Instr,
    // The data associated with this block
    // This is for things that don't fit in the 16 byte limit
    // like initializations and constants
    constants: []Constant,
    initializations: []Initialization,
    fn_args: []FnArgs,
    sets: []types.Type,
};

pub const Field = struct {
    label: ?InternId,
    value: ResultLoc,
};

pub const Initialization = struct {
    ty: ?ResultLoc,
    fields: []Field,
};

pub const Constant = union(enum) {
    int: Int,
    float: Float,
    symbol: InternId,
};

pub const Tir = struct {
    //Block list
    blocks: []BasicBlock,
};
