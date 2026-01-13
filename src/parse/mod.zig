const parser = @import("parser.zig");
const ast = @import("Ast.zig");

pub const init_from_lexer = parser.init_from_lexer;
pub const init = parser.init;
pub const parse = parser.parse;
pub const tests = @import("test.zig");

pub const Ast = struct {
    pub const BinaryExpr = ast.BinaryExpr;
    pub const UnaryExpr = ast.UnaryExpr;
    pub const AccessOperator = ast.AccessOperator;
    pub const Assignment = ast.Assignment;
    pub const Ident = ast.Ident;
    pub const TypeExpr = ast.TypeExpr;
    pub const TypeBinaryOp = ast.TypeBinaryOp;
    pub const TypeBinaryExpr = ast.TypeBinaryExpr;
    pub const TypeIntRange = ast.TypeIntRange;
    pub const TypeSet = ast.TypeSet;
    pub const TypeLabel = ast.TypeLabel;
    pub const TypeStruct = ast.TypeStruct;
    pub const TypeEnum = ast.TypeEnum;
    pub const Path = ast.Path;
    pub const TypeCons = ast.TypeCons;
    pub const TypeDecl = ast.TypeDecl;
    pub const VarDecl = ast.VarDecl;
    pub const GlobalDeclMod = ast.GlobalDeclMod;
    pub const FnModifier = ast.FnModifier;
    pub const FnDecl = ast.FnDecl;
    pub const ModuleDecl = ast.ModuleDecl;
    pub const Import = ast.Import;
    pub const ReturnStmt = ast.ReturnStmt;
    pub const Terminated = ast.Terminated;
    pub const ParamList = ast.ParamList;
    pub const IfStmt = ast.IfStmt;
    pub const WhileLoop = ast.WhileLoop;
    pub const Cast = ast.Cast;
    pub const Ternary = ast.Ternary;
    pub const Block = ast.Block;
    pub const SpanId = ast.SpanId;
    pub const AstNodeId = ast.AstNodeId;
    pub const AstNode = ast.AstNode;
    pub const AstKind = ast.AstKind;
    pub const Ast = ast.Ast;
    pub const AstBuilder = ast.AstBuilder;
};


comptime {
    _ = @import("test.zig");
}
