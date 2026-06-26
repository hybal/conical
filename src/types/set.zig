const std = @import("std");
const common = @import("common");

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
    finite: []const Value,

    label: struct {
        name: InternId,
        value: *Set,
    },

    product: []const *Set,
    @"union": []const *Set,
    intersection: []const *Set,
    difference: struct {
        left: *Set,
        right: *Set,
    },
};

pub const Value = union(enum) {
    int: Int,
    float: Float,
    symbol: InternId,
    @"bool": bool,
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


