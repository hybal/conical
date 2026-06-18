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

pub fn get_operator_overload(op: OverloadOp) []const u8 {
    inline for (OPERATOR_OVERLOAD_MAP) |entry| {
        if (entry.@"0" == op) {
            return entry.@"1";
        }
    }
    unreachable;
}
