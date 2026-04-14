const _common = @import("./common.zig");
const _span = @import("span.zig");
const _path = @import("./path.zig");


pub const DefId = _common.DefId;
pub const TypeId = _common.TypeId;
pub const Ident = _common.Ident;
pub const FileId = _span.FileId;
pub const Span = _span.Span;
pub const Context = _common.Context;

pub const path = struct {
   pub const Path = _path.Path;
};

pub const span = struct {
    pub const Span = _span.Span;
    pub const FileId = _span.FileId;
    pub const FileStore = _span.FileStore;
};
