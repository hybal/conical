const std = @import("std");


pub fn Either(comptime A: type, comptime B: type) type {
    return union(enum) {
        a: A,
        b: B,

        pub fn is_a(self: *const @This()) bool {
            return self.* == .a;
        }

        pub fn is_b(self: *const @This()) bool {
            return self.* == .b;
        }

        pub fn make_a(val: A) @This() {
            return .{
                .a = val,
            };
        }

        pub fn make_b(val: B) @This() {
            return .{
                .b = val
            };
        }

        pub fn make(val: anytype) @This() {
            if (@TypeOf(val) == A) {
                return Either.make_a(val);
            } else {
                return Either.make_b(val);
            }
            
        }

    };
}
