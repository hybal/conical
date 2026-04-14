const std = @import("std");
const diag = @import("diagnostics");

const Path = @import("./path.zig");
const span = @import("span.zig");
const Span = span.Span;


pub const DefId = u64;
pub const TypeId = u64;
pub const FileId = span.FileId;

pub const Ident = struct {
    span: Span,
    value: []const u8,

    pub fn equals(self: *const @This(), other: *const @This()) bool {
        if (self.span.start != other.span.start or self.span.end != other.span.end) {
            return false;
        }
        //technically checking "value" is redundent but is included for completness
        if (self.value.len != other.value.len) {
            return false;
        }
        for (self.value, 0..self.value.len) |val, i| {
            if (val != other.value[i]) {
                return false;
            }
        }
        return true;
    }

    pub fn hash(self: *const @This()) u64 {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(self.value);
        return hasher.final();
    }

};


pub const Context = struct {
    //sym_tab: std.ArrayList(SymbolTable),
    //type_tab: TypeTbl,
    source: []const u8,
    file_path: []const u8,
    file_store: span.FileStore,
    session: diag.ErrorStore,
    //module: ?ModuleId,
    //module_store: *ModuleStore,
};


pub inline fn createWith(allocator: std.mem.Allocator, val: anytype) std.mem.Allocator.Error!*@TypeOf(val) {
    const allocation = try allocator.create(@TypeOf(val));
    allocation.* = val;
    return allocation;
}


