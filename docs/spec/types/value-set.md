# Value Set Semantics

## Validation

Validation between two value-sets is defined as a subset-or-equal check between them.
Formally, given:

- inferred set $I$ 
- expected set $E$

The validation is defined as:

$$
S =
\begin{cases}
E, & \text{if } I \subseteq E \\
\bot, & \text{otherwise}
\end{cases}
$$

Where $\bot$ denotes a type error, and any widening or value conversion required to transform $I$ to $E$ is applied.

## Inference

Inference happens in two phases

1. [Construction](#construction)
2. [Simplification](#simplification)

### Construction

Given an SSA value $v$ that has an associated set expression $E(v)$ and memory slot $S(v)$, with associated value set expression $E(S)$.

The inference starts by constructing $E(v)$ via the following rules:

1. If $v$ is a constant literal then: 
$$
E(v) = v
$$

1. If $v$ is a variable then:

$$
E(v) = E(S) 
$$

1. If $v$ is a primitive operation and given $v = op(l, r)$, then:

$$
E(v) = Prim(op, E(l), E(r))
$$

Inference should then be ran on the two operands to update the slot associated with them.

1. If $v$ is a call to a free-function with the return set $E(R)$ then:

$$
E(v) = E(R)
$$

Inference should then be ran on all of the arguments, if any of them are SSA variables then the slot associated with those variables should have its set expression updated.

## Collapse

To collapse the generated set expressions these rules are followed:

1. If there is a union of two constants those constants are collapsed: $a \cup b = \{a, b\}$

1. If a term of the union is a reference to an existing set (via SlotExpr(var)) then a subset operation is performed between the current set and the reference, and if the existing set is a subset then the reference set is taken:

$$
S =
\begin{cases}
R, & \text{if } E \subseteq R \\
E \cup R, & \text{otherwise}
\end{cases}
$$

1. If a term of the union is a primitive operation, then the operation is performed on all current set elements if possible. If the current set is not a subset of the required operator's operand sets then it is a type error.

1. If a term of the union is a function application, if the function is known then type check each parameter to the required ones, then substitute the known return set and perform the collapse on that expression. If the function is not known then the parameter types are the ones inferred by the application.

1. If the term is infinity generate an error requiring an explicit annotation on the initial slot construction.

