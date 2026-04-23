pub const std = @import("std");
const Path = @import("path.zig").Path;



pub const FileId = u32;
//This represents a region in the source code
pub const Span = struct {
    start: usize,
    end: usize, //exclusive
    fileid: FileId,

    pub fn init(initial: usize, file: FileId) @This() {
        return .{
            .start = initial,
            .end = initial + 1,
            .fileid = file,
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


pub const File = union(enum) {
     file: Path,
     buffer: []const u8,
     _,
};


pub const FileStore = struct {
    store: std.ArrayList(File),
    gpa: std.mem.Allocator,
    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .store = .empty,
            .gpa = allocator,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.store.deinit(self.gpa);
    }

    pub fn put(self: *@This(), file: File) !FileId {
        const id = self.store.items.len;
        try self.store.append(self.gpa, file);
        return @intCast(id);
    }
};

