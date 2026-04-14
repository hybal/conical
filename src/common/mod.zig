const common = @import("./common.zig");
const _span = @import("span.zig");
const _path = @import("./path.zig");


pub const DefId = common.DefId;
pub const TypeId = common.TypeId;
pub const FileId = common.FileId;
pub const Ident = common.Ident;
pub const Span = _span.Span;
pub const Context = common.Context;

pub const path = struct {
   pub const Path = _path.Path;
};

pub const span = struct {
    pub const Span = _span.Span;
    pub const FileId = _span.FileId;
    pub const FileStore = _span.FileStore;
};
