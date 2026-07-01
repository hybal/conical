const std = @import("std");
const set = @import("./set.zig");
const Set = set.Set;
const common = @import("common");


pub const Closure = struct {
    env: Set,
    signature: FnSignature,

    pub fn equal(self: *const @This(), other: *const @This()) bool {
        if (self.env.compare(&other.env) == .subset) return false;
        if (!self.signature.equal(other.signature)) return false;
        return true;
    }
};


pub const FnSignature = struct {
    args: []Set,
    ret: Set,

    pub fn equal(self: *const @This(), other: *const @This()) bool {
        if (self.args.len != other.args.len) return false;
        if (self.ret.compare(&other.ret) == .subset) return false;
        for (self.args, other.args) |s, o| {
            if (s.compare(&o) == .subset) return false;
        }
        return true;
    }
};

