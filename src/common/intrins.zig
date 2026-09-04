const std = @import("std");


pub const OverloadOp = enum {
    add,
    sub,
    mul,
    div,
    rem,
    negate,
    lshift,
    rshift,
    bitor,
    bitand,
    bitxor,
    lognot,
    logand,
    logor,
    eq,
    neq,
    lt,
    gt,
    lteq,
    gteq,
    unwrap,
    next,
    index,
    index_set,
    slice,
    ref,
    deref,
    deref_set,
};


/// Set of intrinsics 
/// Note: it would be worth considering if the different variants of certain functions (e.g. types of arithmetic),
///  should be seperate intrinsics, or a single intrinsic with a mode parameter.
///  For now all forms are listed as seperate intrinsics
pub const Intrinsic = enum {
    /// Integer addition, trapping
    add_it,
    /// Integer addition, wrapping
    add_im,
    /// Integer addition, saturating
    add_is,
    /// Integer subtraction
    sub_it,
    sub_im,
    sub_is,
    /// Integer multiplication
    mul_it,
    mul_im,
    mul_is,
    /// Integer division, truncate
    div_it,
    /// Integer division, floor
    div_if,
    /// Integer division, round
    div_ir,
    /// Integer division, ceiling
    div_ic,
    /// Floating point addition
    add_f,
    /// Floating point subtraction
    sub_f,
    /// Floating point multiplication
    mul_f,
    /// Floating point division
    div_f,
    import,




};

pub const OPERATOR_OVERLOAD_MAP = [_]struct {OverloadOp, []const u8} {
    .{ .add,             "__operator_add"                 },
    .{ .sub,             "__operator_subtract"            },
    .{ .mul,             "__operator_multiply"            },
    .{ .div,             "__operator_divide"              },
    .{ .rem,             "__operator_remainder"           },
    .{ .negate,          "__operator_negate"              },
    .{ .lshift,          "__operator_left_shift"          },
    .{ .rshift,          "__operator_right_shift"         },
    .{ .bitor,           "__operator_bitwise_or"          },
    .{ .bitand,          "__operator_bitwise_and"         },
    .{ .bitxor,          "__operator_bitwise_xor"         },
    .{ .lognot,          "__operator_logical_not"         },
    .{ .logand,          "__operator_logical_and"         },
    .{ .logor,           "__operator_logical_or"         },
    .{ .eq,              "__operator_eq"                  },
    .{ .neq,             "__operator_neq"                 },
    .{ .lt,              "__operator_less_then"           },
    .{ .gt,              "__operator_greater_then"        },
    .{ .lteq,            "__operator_less_then_equal"     },
    .{ .gteq,            "__operator_greater_then_equal"  },
    .{ .unwrap,          "__operator_unwrap"              },
    .{ .next,            "__next",                        },
    .{ .index,           "__operator_index",              },
    .{ .index_set,       "__operator_index_set",          },
    .{ .slice,           "__operator_slice",              },
    .{ .ref,             "__operator_ref",                },
    .{ .deref,           "__operator_dereference",        },
    .{ .deref_set,       "__operator_dereference_set",    },
};


pub const INTRINSIC_MAP = [_]struct {Intrinsic, []const u8} {


};




pub fn get_operator_overload(op: OverloadOp) []const u8 {
    inline for (OPERATOR_OVERLOAD_MAP) |entry| {
        if (entry.@"0" == op) {
            return entry.@"1";
        }
    }
    unreachable;
}
