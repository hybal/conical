const std = @import("std");

pub const Int = std.math.big.int.Managed;


/// Represents a IEE-754 inspired 512 bit floating point number.
/// Note that there are no special values like NaN or infinities, the only special case are subnormals.
pub const Float = packed struct {
    sign: u1,
    mantissa: u19, 
    exp: u492,

    pub const FloatError = error {
        Overflow,
    };

    const MANTISSA_BITS = 492;
    const EXPONENT_BITS = 19;
    const SIGN_BITS = 1;
    const EXPONENT_MASK = (1 << EXPONENT_BITS) - 1;
    const MANTISSA_MASK = (1 << MANTISSA_BITS) - 1;
    const BIAS = 262143;
    pub fn init() @This() {
        return .{
            .sign = 0,
            .mantissa = 0,
            .exp = 0,
        };
    }

//    pub fn init_string(allocator: std.mem.Allocator, chars: []const u8) !@This() {
//        var self = @This().init(allocator);
//        try self.set_string(chars);
//        return self;
//    }

    fn normalize(exp: u492, man: u19) FloatError!struct { u492, u19 } {
        var iman = man;
        var iexp = exp;
        if (iman == 0) return .{0, 0};

        while (iman >= (1 << (MANTISSA_BITS + 1))) {
            iman = round_shift_right(man, 1);
            iexp = iexp + 1;
        }

        while (iman < (1 << MANTISSA_BITS)) {
            iman = iman << 1;
            iexp = iexp - 1;
        }
        if (iexp >= EXPONENT_MASK) {
            return FloatError.Overflow;
        }

        if (iexp <= 0) {
            const shift = 1 - iexp;
            iman = round_shift_right(man, shift);
            iexp = 0;
            return .{iexp, iman};
        }

        iman = iman & MANTISSA_MASK;
        return .{ iexp + BIAS, iman};
    }

    fn round_shift_right(man: u492, n: u492) u492 {
        var iman = man;
        if (n == 0) return iman;
        const half = 1 << (n - 1);
        const lost = man & ((1 << n) - 1);
        iman = man >> n;
        if (lost > iman) {
            iman = iman + 1;
        } else if (lost == half) {
            if (iman & 1 == 1) iman = iman + 1;
        }
        return iman;
    }

    pub fn add(self: @This(), other: @This()) FloatError!@This() {
        var a_man: u32 = self.mantissa;
        var b_man: u32 = other.mantissa;
        var a_exp = self.exp;
        var b_exp = other.exp;
        const a_sign = self.sign;
        const b_sign = other.sign;
        if (a_exp != 0) a_man = a_man | (1 << MANTISSA_BITS);
        if (b_exp != 0) b_man = b_man | (1 << MANTISSA_BITS);

        a_exp = if (a_exp == 0) 1 else a_exp - BIAS;
        b_exp = if (b_exp == 0) 1 else b_exp - BIAS;

        if (a_exp > b_exp) {
            b_man = round_shift_right(b_man, a_exp - b_exp);
            b_exp = a_exp;
        } else if (b_exp > a_exp) {
            a_man = round_shift_right(a_man, b_exp - a_exp);
            a_exp = b_exp;
        }

        var man: u492 = undefined;
        var sign: u1 = undefined;
        const exp: u19 = a_exp;

        if (a_sign == b_sign) {
            man = a_man + b_man;
            sign = a_sign;
        } else {
            if (a_man >= b_man) {
                man = a_man - b_man;
                sign = a_sign;
            } else {
                man = b_man - a_man;
                sign = b_sign;
            }
        }

        return .{
            .sign = sign,
            .mantissa = man,
            .exp = exp,
        };
    }
};
