# Value Set Semantics

## Validation

Validation between two value-sets is defined as a subset-or-equal check between them.
Formally, given:

- inferred set `I` 
- expected set `E`

The validation is defined as:

$$
S =
\begin{cases}
E, & \text{if } I ⊆ E \\
⊥, & \text{otherwise}
\end{cases}
$$

Where `⊥` denotes a type error, and any widening or value conversion required to transform `I` to `E` is applied.

## Inference

For inference to function every SSA variable in the [MIR](../mir/overview.md) is annotated with the specific memory slot that it refers to. 

The inference algorithm starts by walking the SSA graph and constructing a non-evaluated set expression and assigning said expression to relevant memory slots (or SSA values if they are ephemeral).
It does this by following these rules for assignment (given the inferred set expression E):

1. For a new slot initialize its associated sets to ∅.

1. If it is a constant then add it to the value set via a union: `Ev ∪ c`

1. If it is a reference to an existing SSA variable then add that variable's value set to this one by adding an abstract reference to it: `E ∪ SlotExpr(var)`

1. If it is a primitive operation add that operation as unevaluated: `E ∪ Prim(op, left ∪ right)`, also for each argument to the operator update the corresponding slots with the expected type, that is `right ⊆ left` for most operations.

1. If it is a function call add that call: `E ∪ Call(func, parameters)`, and also add each of the parameter's value sets to the slots that are being referred too.

1. If it is a cyclical operation (via a φ function) then add the special element ∞, which specifies that this set cannot be determined and as such requires an explicit annotation

## Collapse

To collapse the generated set expressions these rules are followed:

1. If there is a union of two constants those constants are collapsed: `a ∪ b = {a, b}`

1. If a term of the union is a reference to an existing set (via SlotExpr(var)) then a subset operation is performed between the current set and the reference, and if the existing set is a subset then the reference set is taken:

$$
S =
\begin{cases}
R, & \text{if } E ⊆ R \\
E | R, & \text{otherwise}
\end{cases}
$$

1. If a term of the union is a primitive operation, then the operation is performed on all current set elements if possible. If the current set is not a subset of the required operator's operand sets then it is a type error.

1. If a term of the union is a function application, if the function is known then type check each parameter to the required ones, then substitute the known return set and perform the collapse on that expression. If the function is not known then the parameter types are the ones inferred by the application.

1. If the term is infinity generate an error requiring an explicit annotation on the initial slot construction.

