const parser = @import("parser.zig");
const ast = @import("Ast.zig");

pub const print = @import("print.zig");


pub const init_from_lexer = parser.init_from_lexer;
pub const init = parser.init;
pub const parse = parser.parse;

pub const Ast = ast; //Once the api stabalizes this should be explicit



test {
    _ = @import("test/tests.zig");
}
