# Set Comparison

There exist builtin comparison operators that compare two sets to each other and exist as "gates" for sets.
That is for a given set `A` and a comparison operator, the two valid outputs are either the original set A or undefined (⊥). 
 
These operators are:

- [Membership](#membership)
- [Subset](#subset)
- [Superset](#superset)
- [Strict Subset](#strict-subset)
- [Strict Superset](#strict-superset)

## Membership

The membership operator is defined by the production [`TYPE_EXPRESSION_MEMBERSHIP`](../syntax/grammar.md#types), and is defined as a membership check for value `a` in set `A`.
Or formally: 

$$
S =
\begin{cases}
A, & \text{if } a ∈ A \\
⊥, & \text{otherwise}
\end{cases}
$$

For example, given the set `{1, 2, 3}` and the value `3` the expression `3 in {1, 2, 3}` evaluates to `{1, 2, 3}` as the condition is true.

## Subset

The subset operator is defined by the production [`TYPE_EXPRESSION_INCLUSION`](../syntax/grammar.md#types), and is defined as:
given the sets `A` and `B` the subset check of A and B is the condition that all elements in A are in B.
Or formally:

$$
S =
\begin{cases}
A, & \text{if } A ⊆ B \\
⊥, & \text{otherwise}
\end{cases}
$$

For example, given the sets `{1, 2, 3}` and `{1, 2}` the subset check of `{1, 2, 3} ⊆ {1, 2}` will evaluate to ⊥ which will cause a type error. 

## Superset

The superset operator is defined by the production [`TYPE_EXPRESSION_INCLUSION`](../syntax/grammar.md#types), and is defined as:
give the sets `A` and `B` the superset check of A and B is the condition that all elements in B are in A.

Or formally:

$$
S =
\begin{cases}
A, & \text{if } A ⊇ B \\
⊥, & \text{otherwise}
\end{cases}
$$

For example, given the sets `{1, 2, 3}` and `{1, 2}` the superset check of `{1, 2, 3} ⊇ {1, 2}` will evaluate to `{1, 2, 3}`.

## Strict Subset

The strict subset operator is defined by the production [`TYPE_EXPRESSION_STRICT_INCLUSION`](../syntax/grammar.md#types) and is defined as:
given the sets `A` and `B` the strict subset check of A and B is the condition that all elements in A are in B but A does not equal B.
Or formally:

$$
S =
\begin{cases}
A, & \text{if } A ⊂ B \\
⊥, & \text{otherwise}
\end{cases}
$$

For example, given the sets `{1, 2}` and `{1, 2}` the strict subset check of `{1, 2} ⊂ {1, 2}` will evaluate to ⊥, which will cause a type error.

## Strict Superset

The strict superset operator is defined by the production [`TYPE_EXPRESSION_STRICT_INCLUSION`](../syntax/grammar.md#types) and is defined as:
given the sets `A` and `B` the strict superset check of A and B is the condition that all elements in B are in A but B does not equal A.
Or formally:

$$
S =
\begin{cases}
A, & \text{if } A ⊃ B \\
⊥, & \text{otherwise}
\end{cases}
$$

For example, given the sets `{1, 2, 3}` and `{1, 2}` the strict superset check of `{1, 2, 3} ⊃ {1, 2}` will evaluate to `{1, 2, 3}`

