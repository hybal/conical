const std = @import("std");
const common = @import("common");
const hir = @import("hir");
const types = @import("types");
const eval = @import("./eval.zig");




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
    // Set membership
    membership,
    // Set subset-or-equal
    subeq,
    // Set superset-or-equal
    supeq,
    // Set subset
    sub,
    // Set superset,
    sup,
    // Type check, checks a inferred type against a computed one.
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
    // Unconditional block-based jump, CANNOT be used for arbitrary jumps to code locations
    jmp,
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
    // Load a value from a global symbol
    load_global,
    // Store a value to a global symbol
    store_global,
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
    // Tries to evaluate a block, in-which case gets replace with the comptime result
    eval,
    // Construct an initializer
    init,
    _,
};

/// A single Tir instruction
pub const Instr = union(InstrKind) {
    // Stored inline since its cheaper
    bool: bool,
    constant: ConstantRef,
    @"union": struct { left: ResultLoc, right: ResultLoc },
    product: struct { left: ResultLoc, right: ResultLoc },
    difference: struct { left: ResultLoc, right: ResultLoc },
    intersection: struct { left: ResultLoc, right: ResultLoc },
    membership: struct { left: ResultLoc, right: ResultLoc },
    land: struct { left: ResultLoc, right: ResultLoc },
    lor: struct { left: ResultLoc, right: ResultLoc },
    lnot: ResultLoc,
    arg_get: ArgIndex,
    call: struct { func: ResultLoc, args: FnArgRef },
    jmp: BlockId,
    br: struct { cond: ResultLoc, then: BlockId, @"else": BlockId },
    make_slot: ResultLoc,
    access: struct { ResultLoc, InternId },
    load: ResultLoc,
    store: struct { ResultLoc, ResultLoc },
    load_global: Symbol,
    store_global: struct { sym: Symbol, value: ResultLoc },
    load_ptr: ResultLoc,
    store_ptr: ResultLoc,
    slot_ptr: ResultLoc,
    yield: ResultLoc,
    eval: BlockId,
    init: InitRef,
};

pub const InitRef = u32;
pub const FnArgRef = u32;

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
    // The containing block of this block
    //  used mostly so that evaluation / analysis doesn't have to keep state
    parent: BlockId,
    // The range in the instruction list
    instructions: Range,
    // The data associated with this block
    // This is for things that are variable width,
    //  like initializations and constants
    constants: []Constant,
    initializations: []Initialization,
    fn_args: []FnArgs,
    sets: []types.Type,
    // Arguments to this block, equivalent to a phi function in SSA
    // NOTE: may be removed
    args: []Expr,
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

pub const Visibility = enum {
    @"pub",
    private,
};

pub const Linkage = enum {
    @"extern",
    @"export",
    internal,
};

pub const Expr = struct {
    block: BlockId,
    value: ResultLoc,
};

pub const Function = struct {
    block: BasicBlock,
    params: []Expr,
    generics: []?Expr,
    ret: Expr,
    sym: Symbol,
    visibility: Visibility,
    linkage: Linkage,
    is_pure: bool,
    is_comptime: bool,
    is_inline: bool,
};

pub const Binding = struct {
    sym: Symbol,
    expr: Expr,
    is_alias: bool,
    is_mut: bool,
    is_move: bool,
};

/// Result of either a compile-time computation or a runtime computation
pub const Result = union(enum) {
    value: eval.CValue,
    instr: usize,
};

/// Map of ResultLoc -> Result where Result is either a computed value or a runtime instruction.
pub const ResultLocMap = std.AutoHashMap(ResultLoc, Result);

pub const Tir = struct {
    //Block list
    functions: []Function,
    //Bindings
    bindings: []Binding,
    // Full list of instructions for this module, includes every block and isolated expression.
    instructions: []Instr,

    blocks: []BasicBlock,

    resultmap: ResultLocMap,
};

pub const TirBuilder = struct {
    functions: std.ArrayList(Function),
    bindings: std.ArrayList(Binding),
    instructions: std.ArrayList(Instr),
    blocks: std.ArrayList(BasicBlock),
    resultmap: ResultLocMap,
    allocator: std.mem.Allocator,

    pub const BlockBuilder = struct {
        self: *TirBuilder,
        constants: std.ArrayList(Constant),
        initializations: std.ArrayList(Initialization),
        fn_args: std.ArrayList(FnArgs),
        sets: std.ArrayList(types.Type),
        start: usize,
        end: usize,
    };

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .functions = .empty,
            .bindings = .empty,
            .instructions = .empty,
            .resultmap = .init(allocator),
            .allocator = allocator,
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
