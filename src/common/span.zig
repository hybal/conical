pub const std = @import("std");
pub const Path = @import("path.zig").Path;
pub const FileId = u32;
//This represents a region in the source code
pub const Span = struct {
    start: usize,
    end: usize, //exclusive
    //fileid: FileId, // In-Progress

    pub fn init(initial: usize) @This() {
        return .{
            .start = initial,
            .end = initial + 1,
        };
    }

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


// Will eventually also have macro expansion and other types
pub const File = union(enum) {
     file: Path,
     _,
};

pub const FileStore = struct {
    store: std.ArrayList(File),
    map: std.StringHashMap(FileId), //Maps fully normalized paths to fileid's

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .store = .init(allocator),
            .map = .init(allocator),
        };
    }

    pub fn put(self: *@This(), file: Path) !FileId {
        const id = self.store.items.len;
        try self.store.append(.{ .file = file });
        try self.map.put(file.normalize(), id);
        return id;
    }
};

