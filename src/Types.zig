

const TypeExpr = union(enum) {
    @"union": *Union,
    product: *Product,
    subset: *Subset,
    superset: *Superset,
    expr: *Expr
};

const Union = struct {
    left: Type,
    left_tag: ?[]const u8,
    right: Type,
    right_tag: ?[]const u8,
};

const Product = struct {
    left: Type,
    left_tag: ?[]const u8,
    right: Type,
    right_tag: []const u8,
};

const Subset = struct {
    left: Type,
    right: Type,
};

const Superset = struct {
    left: Type,
    right: Type,
};

const Range = struct {
    start: usize,
    end: usize,
};

const Expr = union(enum) {
    id: []const u8, //change to symbolid
    range: *Range,
    array: *Array,
    slice: *Slice,
    function: *FuncType,
};

