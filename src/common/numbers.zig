/// Large and Arbitrary precision numbers for compile-time execution
/// TODO: add tests
/// TODO: make custom large int implementation
const std = @import("std");

const int = std.math.big.int;

pub const Int = std.math.big.int.Managed;

const PRECISION = 148;
const MAX_MANTISSA = std.math.maxInt(i493);
const MIN_MANTISSA = std.math.minInt(i493);
const MAX_EXPONENT = std.math.maxInt(i19);
const MIN_EXPONENT = std.math.minInt(i19);

/// A 512 base-10 floating point number
/// Note: this is incredibly slow on debug builds
pub const Float = packed struct {
    mantissa: i493, 
    exp: i19,

    pub const FloatError = error {
        Overflow,
    };

    pub fn init(mantissa: i493, exponent: i19) @This() {
        return (@This() {
            .mantissa = mantissa,
            .exp = exponent,
        }).normalize();
    }

    /// Convert a string to a float
    /// Follows the following format:
    /// [+-]? ([1..9] [0..9]* | '0') '.' [0..9]* ( [eE] ([+-]?) [0..9]+ )
    pub fn init_str(str: []const u8) !@This() {
        var mantissa: i986 = 0;
        var exp: i19 = 0;
        var sign: i986 = 1;

        var seen_dot = false;
        var i: usize = 0;

        if (str.len == 0)
            return error.InvalidNumber;

        if (str[0] == '-') {
            sign = -1;
            i += 1;
        } else if (str[0] == '+') {
            i += 1;
        }

        while (i < str.len) : (i += 1) {
            const c = str[i];

            switch (c) {
                '0'...'9' => {
                    mantissa *= 10;
                    mantissa += c - '0';

                    if (seen_dot) exp -= 1;
                },

                '.' => {
                    if (seen_dot) return error.InvalidNumber;
                    seen_dot = true;
                },

                'e', 'E' => {
                    i += 1;
                    const extra_exp =
                        try std.fmt.parseInt(
                            i19,
                            str[i..],
                            10,
                        );

                    exp += extra_exp;
                    break;
                },

                else => return error.InvalidNumber,
            }
        }

        mantissa *= sign;

        return @This().normalize_wide(
            mantissa,
            exp,
        );    
    }

    fn char_to_int(char: u8) u8 {
        return char - 30;
    }



    fn pow10(comptime T: type, n: usize) T {
        return std.math.pow(T, 10, n);
    }

    fn count_digits(v: anytype) u16 {
        if (v == 0) return 1;

        var n = @abs(v);
        var digits: u16 = 0;

        while (n != 0) {
            n /= 10;
            digits += 1;
        }

        return digits;
    }
    fn normalize(self: @This()) @This() {
        return normalize_wide(@intCast(self.mantissa), self.exp);
    }
    fn normalize_wide(_mantissa: i986, _exp: i19) @This() {
        if (_mantissa == 0) {
            return .{
                .mantissa = 0,
                .exp = 0,
            };
        }

        var mantissa = _mantissa;
        var exp = _exp;

        const digits = count_digits(mantissa);

        if (digits > PRECISION) {
            const shift = digits - PRECISION;

            mantissa = @divTrunc(
                mantissa,
                std.math.pow(i986, 10, shift),
            );

            exp += @intCast(shift);
        } else if (digits < PRECISION) {
            const shift = PRECISION - digits;

            mantissa *= std.math.pow(
                i986,
                10,
                shift,
            );

            exp -= @intCast(shift);
        }

        return .{
            .mantissa = @intCast(mantissa),
            .exp = exp,
        };    
    }   

    pub fn add(self: @This(), other: @This()) @This() {
        if (self.exp == other.exp) {
            const new = @This() { .mantissa = self.mantissa + other.mantissa, .exp = self.exp };
            return new;
        }

        var exp = @max(self.exp, other.exp);
        var mantissa = if (self.exp > other.exp) self.mantissa else other.mantissa;
        while (exp != @min(self.exp, other.exp)) {
            mantissa = @divTrunc(mantissa, 10);
            exp -= 1;
        }
        const new_mant = mantissa + if (self.exp < other.exp) self.mantissa else other.mantissa;
        return .init(new_mant, exp);
    }

    pub fn sub(self: @This(), other: @This()) @This() {
        const neg_other = other.negate();
        const out = self.add(neg_other);
        return out;
    }

    pub fn negate(self: @This()) @This() {
        return .init(-self.mantissa, self.exp);
    }

    pub fn mul(self: @This(), other: @This()) @This() {
        const mantissa = self.mantissa * other.mantissa;
        const exp = self.exp + other.exp;
        return .init(mantissa, exp);
    }

    pub fn div(self: @This(), other: @This()) @This() {
        const scale = std.math.pow(i986, 10, PRECISION - 1);

        const m: i986 = 
            @divTrunc((@as(i986, self.mantissa) * scale),
                @as(i986, other.mantissa));

        const exp = self.exp - other.exp - (PRECISION - 1);
        return Float.normalize_wide(m, exp);
    }

    pub fn abs(self: @This()) @This() {
        return .{ .mantissa = @abs(self.mantissa), .exp = self.exp };
    }

    pub fn compare(self: @This(), other: @This()) std.math.Order {
        if (self.exp > other.exp) return .gt;
        if (self.exp < other.exp) return .lt;
        if (self.mantissa > other.mantissa) return .gt;
        if (self.mantissa < other.mantissa) return .lt;
        return .eq;
    }


    pub fn to_string(_self: @This(), allocator: std.mem.Allocator) ![]u8 {
        const self = _self.normalize();
        const absv = if (self.mantissa < 0) -self.mantissa else self.mantissa;

        var most_sig = absv;
        var digits: i19 = 0;
        while (most_sig >= 10) {
            most_sig = @divTrunc(most_sig, 10);
            digits += 1;
        }

        const remainder = absv - most_sig * blk: {
            var p: i493 = 1;
            var i: i19 = 0;
            while (i < digits) : (i += 1) p *= 10;
            break :blk p;
        };

        const adjusted_exp = self.exp + (PRECISION - 1);
        if (self.mantissa < 0) {
            return std.fmt.allocPrint(allocator, "-{}.{}e{}", .{ most_sig, remainder, adjusted_exp});
        } else {
            return std.fmt.allocPrint(allocator, "{}.{}e{}", .{ most_sig, remainder, adjusted_exp });
        }
    }
};
