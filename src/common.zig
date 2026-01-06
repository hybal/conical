const std = @import("std");
const Ast = @import("Ast.zig");
const Hir = @import("Hir.zig");
const diag = @import("diag.zig");

pub inline fn createWith(allocator: std.mem.Allocator, val: anytype) std.mem.Allocator.Error!*@TypeOf(val) {
    const allocation = try allocator.create(@TypeOf(val));
    allocation.* = val;
    return allocation;
}

pub const DefId = u64;
pub const TypeId = u64;

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

//This represents a region in the source code
pub const Span = struct {
    //TODO: add file id
    start: usize,
    end: usize, //exclusive
    //provided the source code, will get the actual string that this span represents
    pub fn get_string(self: *const Span, source: []const u8) []const u8 {
        return source[self.start..self.end];
    }
    //combines two spans
    pub fn merge(self: *@This(), other: Span) void {
        self.start = @min(self.start, other.start);
        self.end = @max(self.end, other.end);
    }

};

//pub const ModuleId = u64;
//
//pub const Module = struct {
//    exports: std.HashMap(DefId, Symbol, std.hash_map.AutoContext(DefId), 80),
//    path: Ast.Path,
//    source_file: []const u8,
//    imports: std.ArrayList(ModuleId),
//    //TODO: add llvm or paths to object files once full compliation is done
//    pub fn get_symbol(self: *@This(), defid: DefId) ?Symbol {
//        if (self.symbols.get(defid)) |val| {
//            return val;
//        }
//        return null;
//    }
//
//    pub fn hash(self: *const @This()) u64 {
//        var hasher = std.hash.Fnv1a_64.init();
//        hasher.update(std.mem.asBytes(&self.path.hash()));
//        return hasher.final();
//    }
//};
//
//
//pub const ModuleTrie = struct {
//    children: std.StringHashMap(ModuleTrie),
//    value: ?ModuleId, 
//};
//
//pub const ModuleStore = struct {
//    store: std.AutoHashMap(ModuleId, Module),
//    trie: ModuleTrie,
//    lock: std.Thread.RwLock.Impl,
//
//    pub fn get(self: *@This(), modid: ModuleId) ?Module {
//        self.lock.lockShared();
//        const out = self.store.get(modid);
//        self.lock.unlockShared();
//        return out;
//    }
//
//    pub fn put(self: *@This(), modid: ModuleId, mod: Module) !void {
//        self.lock.lock();
//        try self.store.put(modid, mod);
//        self.lock.unlock();
//    }
//};

pub const Context = struct {
    //sym_tab: std.ArrayList(SymbolTable),
    //type_tab: TypeTbl,
    source: []const u8,
    file_path: []const u8,
    session: diag.ErrorStore,
    //module: ?ModuleId,
    //module_store: *ModuleStore,
};





