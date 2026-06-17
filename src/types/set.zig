const std = @import("std");
const common = @import("common");

const Symbol = common.intern.InternId;

const Int = u128;
const Float = f64;

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

pub const SetValue = union(enum) {
    int: Int,
    float: Float,
    symbol: Symbol,
};

pub const Set = struct {
    ints: std.ArrayList(IRange),
    floats: std.ArrayList(FRange),
    symbols: std.ArrayList(Symbol),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) @This() {
        return Set {
            .ints = .empty,
            .floats = .empty,
            .symbols = .empty,
            .allocator= allocator,
        };
    }

    pub fn putV(self: *@This(), value: SetValue) !void {
        switch (value) {
            .int => |v| try self.putIRange(IRange {
                .start = v,
                .end = v,
                .start_inclusive = true,
                .end_inclusive = true,
            }),
            .float => |v| try self.putFRange(FRange {
                .start = v,
                .end = v,
                .start_inclusive = true,
                .end_inclusive = true,
            }),
            .symbol => |v| {
                if (!self.is_member(value)) {
                    try self.symbols.append(self.allocator, v);
                    std.sort.block(Symbol, self.symbols.items, {}, symbolLt);
                }
            }
        }
    }

    fn mergeI(a: IRange, b: IRange) ?IRange {
        if (a.compare(b) == .subset) {
            return b;
        }
        if (b.compare(a) == .subset) {
            return a;
        }

        if (a.overlap(b)) {
            return IRange {
                .start = @min(a.start, b.start),
                .start_inclusive = a.start_inclusive or b.start_inclusive,
                .end = @max(a.end, b.end),
                .end_inclusive = a.end_inclusive or b.end_inclusive,
            };
        }

        if (a.start_inclusive or b.end_inclusive) {
            if (a.end < b.start
                and a.end + 1 == b.start) return IRange {
                .start = a.start,
                .start_inclusive = a.start_inclusive,
                .end = b.end,
                .end_inclusive = b.end_inclusive,
            };

            if (b.end < a.start
                and b.end + 1 == a.start) {
                return IRange {
                    .start = b.start,
                    .start_inclusive = b.start_inclusive,
                    .end = a.end,
                    .end_inclusive = a.end_inclusive,
                };
            }
        }
        return null;
    }

    fn mergeF(a: FRange, b: FRange) ?FRange {
        if (a.compare(b) == .subset) {
            return b;
        }
        if (b.compare(a) == .subset) {
            return a;
        }
        if (a.overlap(b)) {
            return FRange {
                .start = @min(a.start, b.start),
                .start_inclusive = a.start_inclusive or b.start_inclusive,
                .end = @max(a.end, b.end),
                .end_inclusive = a.end_inclusive or b.end_inclusive,
            };
        }
        return null;
    }

    pub fn putIRange(self: *@This(), range: IRange) !void {
        var done: bool = false;
        for (self.ints.items) |*v| {
            if (mergeI(v.*, range)) |m| {
                v.* = m;
                done = true;
                break;
            }
        }
        if (!done) {
            try self.ints.append(self.allocator, range);
        }

        std.sort.block(IRange, self.ints.items, {}, intRangeLt);
    }

    pub fn putFRange(self: *@This(), range: FRange) !void {
        var done: bool = false;
        for (self.floats.items) |*v| {
            if (mergeF(v.*, range)) |m| {
                v.* = m;
                done = true;
                break;
            }
        }
        if (!done) {
            try self.floats.append(self.allocator, range);
        }
        std.sort.block(FRange, self.floats.items, {}, floatRangeLt);
    }

    pub fn is_member(self: *const @This(), value: SetValue) bool {
        const index = switch (value) {
            .int => |v| std.sort.binarySearch(IRange, self.ints.items, v, intCompare),
            .float => |v| std.sort.binarySearch(FRange, self.floats.items, v, floatCompare),
            .symbol => |v| std.sort.binarySearch(Symbol, self.symbols.items, v, symbolCompare),
        };
        return index != null;
    }

    pub fn is_member_irange(self: *const @This(), range: IRange) bool {
        const idx = std.sort.binarySearch(IRange, self.ints.items, range, iRangeContains);
        return idx != null;
    }

    pub fn is_member_frange(self: *const @This(), range: FRange) bool {
        const idx = std.sort.binarySearch(FRange, self.floats.items, range, fRangeContains);
        return idx != null;
    }

    pub fn @"union"(self: *@This(), other: *const @This()) !void {
        for (other.ints.items) |it| {
            if (!self.is_member_irange(it)) {
                try self.putIRange(it);
            }
        }

        for (other.floats.items) |it| {
            if (!self.is_member_frange(it)) {
                try self.putFRange(it);
            }
        }

        for (other.symbols.items) |it| {
            if (!self.is_member(.{ .symbol = it })) {
                try self.putV(.{ .symbol = it });
            }
        }
    }

    pub fn intersection(self: *@This(), other: *const @This()) !void {
        var ints: std.ArrayList(IRange) = .empty;
        var floats: std.ArrayList(FRange) = .empty;
        var symbols: std.ArrayList(Symbol) = .empty;

        for (self.ints.items) |sr| {
            for (other.ints.items) |or_| {
                if (!sr.overlap(or_)) continue;
                const start_include = 
                    if (sr.start == or_.start) sr.start_inclusive and or_.start_inclusive 
                    else if (sr.start > or_.start) sr.start_inclusive 
                    else or_.start_inclusive;
                const end_include = 
                    if (sr.end == or_.end) sr.end_inclusive and or_.end_inclusive 
                    else if (sr.end > or_.start) sr.start_inclusive 
                    else or_.start_inclusive;
                try ints.append(self.allocator, IRange {
                    .start = @max(sr.start, or_.start),
                    .start_inclusive = start_include,
                    .end = @min(sr.end, or_.min),
                    .end_inclusive = end_include,
                });
            }
        }
        self.ints.deinit(self.allocator);
        self.ints = ints;

        for (self.floats.items) |sr| {
            for (other.floats.items) |or_| {
                if (!sr.overlap(or_)) continue;
                const start_include = 
                    if (sr.start == or_.start) sr.start_inclusive and or_.start_inclusive 
                    else if (sr.start > or_.start) sr.start_inclusive 
                    else or_.start_inclusive;
                const end_include = 
                    if (sr.end == or_.end) sr.end_inclusive and or_.end_inclusive 
                    else if (sr.end > or_.start) sr.start_inclusive 
                    else or_.start_inclusive;
                try floats.append(self.allocator, FRange {
                    .start = @max(sr.start, or_.start),
                    .start_inclusive = start_include,
                    .end = @min(sr.end, or_.min),
                    .end_inclusive = end_include,
                });
            }
        }

        self.floats.deinit(self.allocator);
        self.floats = floats;

        for (self.symbols.items) |ss| {
            const idx = std.sort.binarySearch(Symbol, other.symbols.items, ss, symbolCompare);
            if (idx != null) {
                try symbols.append(self.allocator, ss);
            }
        }

        self.symbols.deinit(self.allocator);
        self.symbols = symbols;
    }

    pub fn difference(self: *@This(), other: *const @This()) !void {
        var ints: std.ArrayList(IRange) = .empty;
        var floats: std.ArrayList(FRange) = .empty;
        var symbols: std.ArrayList(Symbol) = .empty;

    }

    pub fn compare(self: *const @This(), other: *const @This()) SetOrder {
        var self_has_extra = false;
        var other_has_extra = false;

        for (self.ints.items) |sr| {
            const idx = std.sort.binarySearch(IRange, other.ints.items, sr, iRangeContains);
            if (idx == null or sr.compare(other.ints.items[idx.?]) == .superset) {
                self_has_extra = true;
                break;
            }
        }

        for (other.ints.items) |or_| {
            const idx = std.sort.binarySearch(IRange, self.ints.items, or_, iRangeContains);
            if (idx == null or or_.compare(self.ints.items[idx.?]) == .superset) {
                other_has_extra = true;
                break;
            }
        }

        if (!self_has_extra) {
            for (self.floats.items) |sf| {
                const idx = std.sort.binarySearch(FRange, other.floats.items, sf, fRangeContains);
                if (idx == null or sf.compare(other.floats.items[idx.?]) == .superset) {
                    self_has_extra = true;
                    break;
                }
            }
        }

        if (!other_has_extra) {
            for (other.floats.items) |of_| {
                const idx = std.sort.binarySearch(FRange, self.floats.items, of_, fRangeContains);
                if (idx == null or of_.compare(self.floats.items[idx.?]) == .superset) {
                    other_has_extra = true;
                    break;
                }
            }
        }

        if (!self_has_extra) {
            for (self.symbols.items) |ss| {
                if (std.sort.binarySearch(Symbol, other.symbols.items, ss, symbolCompare) == null) {
                    self_has_extra = true;
                    break;
                }
            }
        }

        if (!other_has_extra) {
            for (other.symbols.items) |os| {
                if (std.sort.binarySearch(Symbol, self.symbols.items, os, symbolCompare) == null) {
                    other_has_extra = true;
                    break;
                }
            }
        }

        if (!self_has_extra and !other_has_extra) return .equal;
        if (!self_has_extra) return .subset;
        return .superset;
    }



    pub fn to_string(self: *const @This(), ctx: *common.Context, writer: *std.Io.Writer) !void {
        try writer.print("{{", .{});
        for (self.ints.items) |int| {
            if (int.start_inclusive) {
                try writer.print(" [", .{});
            } else {
                try writer.print(" (", .{});
            }
            try writer.print("{}, {}", .{int.start, int.end});
            if (int.end_inclusive) {
                try writer.print("]", .{});
            } else {
                try writer.print(")", .{});
            }
            try writer.print(" ", .{});
        }
        try writer.print("}}", .{});
        try writer.print(", {{", .{});
        for (self.floats.items) |float| {
            if (float.start_inclusive) {
                try writer.print(" [", .{});
            } else {
                try writer.print(" (", .{});
            }
            try writer.print("{}, {}", .{float.start, float.end});
            if (float.end_inclusive) {
                try writer.print("]", .{});
            } else {
                try writer.print(")", .{});
            }
            try writer.print(" ", .{});
        }
        try writer.print("}}", .{});
        try writer.print(", {{ ", .{});
        for (self.symbols.items) |symbol| {
            try writer.print(".{s} ", .{ctx.intern_pool.get(symbol).?});
        }
        try writer.print("}}", .{});
    }
};

