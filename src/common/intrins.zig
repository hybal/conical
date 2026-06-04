const std = @import("std");


pub const OverloadOp = enum {
    add,
    sub,
    mul,
    div,
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
    logand,
    logor,
    lognot,
    unwrap,
    next,
    index,
    index_set,
    slice,
};

pub const OPERATOR_OVERLOAD_MAP = [_]struct {OverloadOp, []const u8} {
    .{ .add,             "__operator_add"         },
    .{ .sub,             "__operator_subtract"    },
    .{ .mul,             "__operator_multiply"    },
    .{ .div,             "__operator_divide"      },
    .{ .lshift,          "__operator_left_shift"  },
    .{ .rshift,          "__operator_right_shift" },
    .{ .bitor,           "__operator_bitwise_or"  },
    .{ .bitand,          "__operator_bitwise_and" },
    .{ .bitxor,          "__operator_bitwise_xor" },
    .{ .eq,              "__operator_eq"          },
    .{ .neq,             "__operator_neq"         },
    .{ .lt,              "__operator_bitwise_xor" },
    .{ .gt,              "__operator_bitwise_xor" },
    .{ .lteq,            "__operator_bitwise_xor" },
    .{ .gteq,            "__operator_bitwise_xor" },
    .{ .logand,          "__operator_logical_and" },
    .{ .logor,           "__operator_logical_or"  },
    .{ .lognot,          "__operator_logical_not" },
    .{ .unwrap,          "__operator_unwrap"      },
    .{ .next,            "__next",                },
    .{ .index,           "__operator_index",      },
    .{ .index_set,       "__operator_index_set",  },
    .{ .slice,           "__operator_slice",      },
};

pub fn get_operator_overload(op: OverloadOp) []const u8 {
    inline for (OPERATOR_OVERLOAD_MAP) |entry| {
        if (entry.@"0" == op) {
            return entry.@"1";
        }
    }
    unreachable;
}
