const std = @import("std");
const common = @import("common");
const Closure = @import("./closure.zig").Closure;

const Symbol = common.intern.InternId;

const Int = common.numbers.Int;
const Float = common.numbers.Float;

const InternId = common.intern.InternId;


pub const Set = union(enum) {
    empty,
    anyint,
    anyfloat,
    universal,
    ptr,
    @"type",
    relevant: *Set,
    finite: []const Value,

    label: struct {
        name: InternId,
        value: *Set,
    },

    product: []const Set,
    @"union": []const Set,
    intersection: []const Set,
    difference: struct {
        left: *Set,
        right: *Set,
    },
    associative: []const Set,

    default: struct {
        set: *Set,
        expr: Value,
    },
    // It might be a good idea for ranges to actually be apart of the value type
    // but that remains to be seen
    irange: IRange,
    frange: FRange,
    pub fn new() @This() {
        return .empty;
    }

    pub fn initScalar(allocator: std.mem.Allocator, value: Value) !Set {
        var arr: []Value = try allocator.alloc(Value, 1);
        arr[0] = value;
        return .{
            .finite = arr,
        };
    }

    pub fn initIRange(irange: IRange) Set {
        return .{
            .irange = irange,
        };
    }

    pub fn initFRange(frange: FRange) Set {
        return .{
            .frange = frange,
        };
    }

    pub fn initSpecial(value: enum { anyint, anyfloat, universal, ptr, @"type" }) Set {
        const out: Set = switch (value) {
            .anyint => .anyint,
            .anyfloat => .anyfloat,
            .universal => .universal,
            .ptr => .ptr,
            .@"type" => .@"type",
        };
        return out;
    }


    pub fn clone(self: *const @This(), allocator: std.mem.Allocator) !@This() {
        var out: @This() = undefined;
        switch (self.*) {
            .empty, .anyint, .anyfloat, .universal, .ptr => out = self.* ,
            .finite => |v| out = .{ .finite = try allocator.dupe(Value, v)},
            .product => |v| out = .{ .product = try allocator.dupe(Set, v) },
            .@"union" => |v| out = .{ .@"union" = try allocator.dupe(Set, v) },
            .intersection => |v| out = .{ .intersection = try allocator.dupe(Set, v) },
            .difference => |v| out = .{ .difference = .{ 
                .left = try common.createWith(allocator, try v.left.clone(allocator)),
                .right = try common.createWith(allocator, try v.right.clone(allocator))
            }},
            .associative => |v| out = .{ .associative = try allocator.dupe(Set, v) },
            .irange => out = self.*,
            .frange => out = self.*,
            .label => |l| {
                out = .{ .label = .{
                    .name = l.name,
                    .value = try common.createWith(allocator, try l.value.clone(allocator)),
                }};
            },
            .@"type" => out = .@"type",
            .default => |d| {
                out = .{ .default = .{
                    .expr = d.value,
                    .set = try d.set.clone(allocator),
                }};
            },
        }
        return out;
    }

    pub fn compare(self: *const @This(), other: *const @This()) SetOrder {
        switch (self.*) {
            .empty => {
                if (other.* != .empty) return .subset;
                return .subset;
            },
            .universal => {
                if (other.* == .universal) return .subset;
                return .superset;
            },
            .anyint => {
                if (other.* == .anyint) return .subset;
                if (other.* == .anyfloat) return .subset;
                return .superset;
            },
            .anyfloat => {
                if (other.* == .anyfloat) return .subset;
                return .superset;
            },
            .ptr => {
                if (other.* == .ptr) return .subset;
                return .superset;
            },
            .@"type" => {
                if (other.* == .@"type") return .equal;
                return .superset;
            },
            .finite => |vs| {
                for (vs) |v| {
                    if (!other.membership(v)) return .superset;
                }
                return .subset;
            },
            .@"union" => |vs| {
                for (vs) |v| {
                    if (v.compare(other) == .superset) return .superset;
                }
                return .subset;
            },
            .product => |vs| {
                if (other.* == .product) {
                    if (vs.len > other.product.len) return .superset;
                    for (vs, 0..) |v, i| {
                        if (v.compare(&other.product[i]) == .superset) return .superset;
                    }
                    return .subset;
                }
            },
            .intersection => |vs| {
                for (vs) |v| {
                    if (v.compare(other) == .superset) return .superset;
                }
                return .subset;
            },
            .difference => |v| {
                if (v.left.compare(other) != .superset
                    and v.right.compare(other) == .superset) return .subset;
                return .superset;
            },
            .default => |v| {
                return v.set.compare(other);
            },
            else => unreachable 
        }
        return .superset;
    }

    pub fn membership(self: *const @This(), val: Value) bool {
        switch (self.*) {
            .empty => return false,
            .universal => return true,
            .anyint => {
                if (val == .int) return true;
                return false;
            },
            .anyfloat => {
                if (val == .float) return true;
                return false;
            },
            .finite => |vs| {
                for (vs) |v| {
                    if (v.equal(&val)) {
                        return true;
                    }
                }
                return false;
            },
            .ptr => return val == .ptr,
            .@"type" => return false,
            .product => |ps| {
                if (val != .composite) return false;
                if (val.composite.len != ps.len) return false;
                for (ps, val.composite) |a, b| {
                    if (!a.membership(b)) return false;
                }
                return true;
            }, 
            .@"union" => |vs| {
                for (vs) |v| {
                    if (v.membership(val)) return true;
                }
                return false;
            },
            .intersection => |vs| {
                for (vs) |v| {
                    if (!v.membership(val)) return false;
                }
            },
            .difference => |vs| {
                if (vs.left.membership(val) and !vs.right.membership(val)) {
                    return true;
                }
                return false;
            },
            .associative => return false,
            .label => |l| return l.value.membership(val),
            .irange => |ir| {
                if (val == .int) {
                    if (ir.start.eql(val.int)) return ir.start_inclusive;
                    if (ir.end.eql(val.int)) return ir.end_inclusive;
                    if (ir.start.order(val.int) == .lt 
                        and ir.end.order(val.int) == .gt) return true;
                }
                return false;
            },
            .frange => |fr| {
                if (val == .float) {
                    if (fr.start.compare(val.float) == .eq) return fr.start_inclusive;
                    if (fr.end.compare(val.float) == .eq) return fr.end_inclusive;
                    if (fr.start.compare(val.float) == .lt
                        and fr.end.compare(val.float) == .gt) return true;
                }
                return false;
            },
            .default => |d| {
                return d.set.membership(val);
            },
        }
        unreachable;
    }


    pub fn union_with(self: *@This(), allocator: std.mem.Allocator, other: *const @This()) !void {
        const other_c = try other.clone(allocator);
        if (self.* == .@"union") {
            var unions = std.ArrayList(Set).empty;
            try unions.appendSlice(allocator, self.@"union");
            try unions.append(allocator, other_c);
            self.@"union" = unions.items;
        } else {
            var unions = std.ArrayList(Set).empty;
            try unions.append(allocator, self.*);
            try unions.append(allocator, other_c);
            self.* = .{ .@"union" = unions.items };
        }
    }

    pub fn intersect_with(self: *@This(), allocator: std.mem.Allocator, other: *@This()) !void {
        const other_c = try other.clone(allocator);
        if (self.* == .intersection) {
            var intersections = std.ArrayList(Set).fromOwnedSlice(self.intersection);
            try intersections.append(allocator, other_c);
            self.intersection = intersections.items;
        } else {
            var intersections = std.ArrayList(Set).empty;
            try intersections.append(allocator, self.*);
            try intersections.append(allocator, other_c);
            self.* = .{ .intersection = intersections.items };
        }
    }

    pub fn product_with(self: *@This(), allocator: std.mem.Allocator, other: *const @This()) !void {
        const other_c = try other.clone(allocator);
        if (self.* == .product) {
            var products = std.ArrayList(Set).fromOwnedSlice(self.product);
            try products.append(allocator, other_c);
            self.product = products.items;
        } else {
            var products = std.ArrayList(Set).empty;
            try products.append(allocator, self.*);
            try products.append(allocator, other_c);
            self.* = .{ .product = products.items };
        }
    }

    pub fn difference_with(self: *@This(), allocator: std.mem.Allocator, other: *const @This()) !void {
        const other_c = try other.clone(allocator);
        if (self.* == .difference) {
            var differences = std.ArrayList(Set).fromOwnedSlice(self.difference);
            try differences.append(allocator, other_c);
            self.difference = differences.items;
        } else {
            var differences = std.ArrayList(Set).empty;
            try differences.append(allocator, self.*);
            try differences.append(allocator, other_c);
            self.* = .{ .difference = differences.items };
        }
    }

    pub fn associative_with(self: *@This(), allocator: std.mem.Allocator, other: *const @This()) !void {
        const other_c = try other.clone(allocator);
        if (self.* == .associative) {
            var associatives = std.ArrayList(Set).fromOwnedSlice(self.associative);
            try associatives.append(allocator, other_c);
            self.associative = associatives.items;
        } else {
            var associatives = std.ArrayList(Set).empty;
            try associatives.append(allocator, self.*);
            try associatives.append(allocator, other_c);
            self.* = .{ .associative = associatives.items };
        }
    }

    pub fn set_default(self: *@This(), value: Value) void {
        self.* = .{ .default = .{
            .set = self.*,
            .expr = value
        }};
    }

};


pub const Value = union(enum) {
    int: Int,
    float: Float,
    symbol: InternId,
    @"bool": bool,
    ptr,
    composite: []Value,
    closure: Closure,

    pub fn equal(self: *const @This(), other: *const @This()) bool {
        switch (self.*) {
            .int => |iv| {
                if (other.* != .int) return false;
                return iv.eql(other.int);
            },
            .float => |fv| {
                if (other.* != .float) return false;
                return fv.compare(other.float) == .eq;
            },
            .symbol => |sym| {
                if (other.* != .symbol) return false;
                return sym == other.symbol;
            },
            .@"bool" => |b| {
                if (other.* != .@"bool") return false;
                return b == other.@"bool";
            },
            .ptr => {
                if (other.* != .ptr) return false;
                return true;
            },
            .composite => |cs| {
                if (other.* != .composite) return false;
                if (other.composite.len != cs.len) return false;
                for (cs, other.composite) |v1, v2| {
                    if (!v1.equal(&v2)) return false;
                }
                return true;
            },
            .closure => |c| {
                if (other.* != .closure) return false;
                if (!c.equal(other.closure)) return false;
                return true;
            }
        }
        return false;
    }
};

pub const SetOrder = enum {
    subset,
    superset,
    equal,
};

pub const IRange = struct {
    start: Int,
    end: Int,
    start_inclusive: bool = true,
    end_inclusive: bool = true,

    pub fn cardinality(self: @This()) Int {
        const start = if (!self.start_inclusive) self.start + 1 else self.start;
        const end = if (!self.end_inclusive) self.end - 1 else self.end;
        if (start > end) return 0;
        return end - start + 1;
    }

    pub fn compare(self: @This(), other: @This()) SetOrder {
        if (self.start > other.start and self.end < other.end) return .subset;
        if (self.start < other.start or self.end > other.end) return .superset;
        if (self.start == other.start 
            and self.end == other.end) {
            const card_self = self.cardinality();
            const card_other = other.cardinality();
            if (card_self == card_other) return .equal;
            if (card_self < card_other) return .subset;
            if (card_self > card_other) return .superset;
        }

        if (self.start_inclusive != other.start_inclusive
            and self.start == other.start) {
            return if (self.start_inclusive) .superset else .subset;
        }
        if (self.end_inclusive != other.end_inclusive
            and self.end == other.end) {
            return if (self.end_inclusive) .superset else .subset;
        }
        return .superset;
    }

    pub fn overlap(self: @This(), other: @This()) bool {
        if (self.end < other.start) return false;
        if (self.start > other.end) return false;
        if (self.start > other.start or self.end < other.end) return true;
        if (self.end == other.start and !(self.end_inclusive or other.start_inclusive)) return false;
        if (self.start == other.end and !(self.start_inclusive or other.end_inclusive)) return false;
        return true;
    }
};

const EPSILON = 0.00000000001;
pub const FRange = struct {
    start: Float,
    end: Float,
    start_inclusive: bool = true,
    end_inclusive: bool = true,

    pub fn compare(self: @This(), other: @This()) SetOrder {
        if (self.start > other.start + EPSILON and self.end + EPSILON < other.end) return .subset;
        if (self.start + EPSILON < other.start or self.end > other.end + EPSILON) return .superset;
        if (self.start == other.start and self.end == other.end and self.start_inclusive == other.start_inclusive and self.end_inclusive == other.end_inclusive) return .equal;
        //We don't check endpoints for any float ranges to ensure correctness.
        //It is less efficient, but it is also more correct
        return .superset;
    }
    pub fn overlap(self: @This(), other: @This()) bool {
        if (self.end < other.start + EPSILON) return false;
        if (self.start > other.end + EPSILON) return false;
        if (self.start > other.start + EPSILON or self.end < other.end + EPSILON) return true;
        return false;
    }
};


fn intRangeCompare(context: void, lhs: IRange, rhs: IRange) std.math.Order {
    _ = context;
    if (lhs.start < rhs.start) return .lt;
    if (lhs.start > rhs.start) return .gt;
    if (lhs.start_inclusive != rhs.start_inclusive) {
        return if (lhs.start_inclusive) .gt else .lt;
    }

    if (lhs.end < rhs.end) return .lt;
    if (lhs.end > rhs.end) return .gt;

    if (lhs.end_inclusive != rhs.end_inclusive) {
        return if (lhs.end_inclusive) .gt else .lt;
    }
    return .eq;
}

fn floatRangeCompare(context: void, lhs: FRange, rhs: FRange) std.math.Order {
    _ = context;
    if (lhs.start < rhs.start) return .lt;
    if (lhs.start > rhs.start) return .gt;
    if (lhs.start_inclusive != rhs.start_inclusive) {
        return if (lhs.start_inclusive) .gt else .lt;
    }

    if (lhs.end < rhs.end) return .lt;
    if (lhs.end > rhs.end) return .gt;

    if (lhs.end_inclusive != rhs.end_inclusive) {
        return if (lhs.end_inclusive) .gt else .lt;
    }
    return .eq;
}

fn intCompare(value: Int, context: IRange) std.math.Order {
    if (value < context.start) return .lt;
    if (value > context.end) return .gt;
    if (context.start_inclusive) {
        if (value == context.start) return .eq;
    }
    if (context.end_inclusive) {
        if (value == context.end) return .eq;
    }
    return .eq;
}

fn floatCompare(value: Float, context: FRange) std.math.Order {
    if (value < context.start) return .lt;
    if (context.start_inclusive) {
        if (value == context.start) return .eq;
    }

    if (value > context.end) return .gt;
    if (context.end_inclusive) {
        if (value == context.end) return .eq;
    }
    return .eq;
}

fn intRangeLt(context: void, lhs: IRange, rhs: IRange) bool {
    return intRangeCompare(context, lhs, rhs) == .lt;
}

fn floatRangeLt(context: void, lhs: FRange, rhs: FRange) bool {
    return floatRangeCompare(context, lhs, rhs) == .lt;
}

fn symbolCompare(lhs: Symbol, rhs: Symbol) std.math.Order {
    return std.math.order(lhs, rhs);
}

fn symbolLt(context: void, lhs: Symbol, rhs: Symbol) bool {
    _ = context;
    return symbolCompare(lhs, rhs) == .lt;
}

fn iRangeContains(context: IRange, range: IRange) std.math.Order {
    return intCompare(context.start, range);
}

fn fRangeContains(context: FRange, range: FRange) std.math.Order {
    return floatCompare(context.start, range);
}


