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

There are two additional cases where validation is recursively applied, all arguments of function calls, and all arguments to primitive operations.
In which case validation is also applied to every argument.

## Inference

Inference happens in two phases

1. [Construction](#construction)
2. [Simplification](#simplification)

### Construction

Given an SSA value $v$ that has an associated set expression $E_v$ and memory slot $S_v$, with associated value set expression $SE_v$.

The inference starts by constructing $E_v$ via the following rules:

1. If $v$ is a constant literal then: 

$$
E_v = v
$$

2. If $v$ is a variable then:

$$
E_v = E_S 
$$

3. If $v$ is a primitive operation and given $v = op(l, r)$, then:

$$
E_v = Prim(op, E_l, E_r)
$$

Inference should then be ran on the two operands to update the slot associated with them.

4. If $v$ is a call to a free-function with the return set $E_R$ then:

$$
E_v = E_R
$$

5. If $v$ is a call to an associated-function $f$ that has not already added to the associated-function set then:
Given the function call: 

$$
f(a_1, \cdots, a_n)
$$

Where $a_1$ is the original expression that was being called with the associated expression, and $E_R$ is the return set expression of the function.

The symbol-scope stack is reverse-walked until an existing type declaration is found that contains an associated function that matches $f$ in both name and signature, with the return set $E_R$.
If none is found, then explicit type information is required.
Otherwise the set-expression becomes:

$$
E_v = E_R
$$

With both free-functions and associated-functions, the argument's are validated with the expected argument type.


6. If $v$ is a phi-function and creates a cycle, then:

$$
E_v = \infty
$$

Where $\infty$ represents an infinite set, or a set that cannot be inferred without explicit type annotations.

7. If $v$ is a phi-function $\phi(a, b)$ and _does not_ create a cycle, then:

$$
E_v = E_a \cup E_b
$$


After the value set has been inferred, it is then added to the memory-slot set. 
That is:

$$
SE_v = SE_V \cup E_v
$$

### Simplification

Given the set expression $E_v$, with the form: $e_1 \cup e_2 \cup \cdot$, and the simplified set: $E_v'$, the following simplification rules are applied recursively until none are applicable.

1. If $e_1$ and $e_2$ are constant literals then:

$$
E_v' = E_v' \cup \lbrace e_1, e_2 \rbrace
$$

2. If $e_1$ and $e_2$ are both integers then they are merged into a range:

$$
E_v' = E_v' \cup Range(min(e_1, e_2), max(e_1, e_2))
$$

3. If $e_1$ is a subset-or-equal of $e_2$ then it is replaced with $e_2$:

$$
E_v' = E_v' \cup e_2
$$

4. If $e_1$ is a superset-or-equal of $e_2$ then:

$$
E_v' = E_v' \cup e_1
$$

5. If $e_1$ and $e_2$ are ranges that overlap then, that is $Range(a_1, b_1)$ and $Range(a_2, b_2)$ then:

$$
E_v' = E_v' \cup Range(min(a_1, a_2), max(b_1, b_2))
$$

6. If $e_1$ is a primitive operator $\circ$, and the primitive operator can be applied to all elements in both sets $E_l$ and $E_r$, then:

$$
E_v' = E_v' \cup \lbrace a \circ b | a \in E_l, b \in E_r \rbrace
$$


Note that simplification can be applied to _any_ set expression, not just those that are inferred.

Note also that further simplification can happen later for optimization purposes.
