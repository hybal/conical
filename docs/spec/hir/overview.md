# HIR Overview

The High-Level Intermediate Representation (HIR) is a subset of the Conical language that has been normalized.

The following rules are defined to lower between [AST](../AST/overview.md) and HIR.

1. For a defined binary operator ($\circ$) and compiler-recognized overload function $\text{overload}$:

   $` \forall a,b. \, a \circ b \mapsto \, \text{a.overload}(\circ) \, a \, b `$


