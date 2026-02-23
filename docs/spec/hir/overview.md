# HIR Overview

The High-Level Intermediate Representation (HIR) is a subset of the Conical language that has been normalized.

The following rules are defined to lower between [AST](../AST/overview.md) and HIR.

- $ \forall a,b. \, a + b \mapsto \, \quad overload(+) a b $
