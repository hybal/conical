# Set Algebra

There exist builtin operations between sets that produce a set.
These are:

- [Union](#union)
- [Cartesian Product](#cartesian-product)
- [Set Difference](#set-difference)
- [Intersection](#intersection)

## Union

The union operator, defined by the production [`TYPE_EXPRESSION_UNION`](../syntax/grammar.md#types), is defined as:

For sets A and B the union between them is all elements of A and all elements of B with de-duplication.
Or formally: `A ∪ B = {x | x ∈ A ∨ x ∈ B}`

For example the union between the set `{1, 2, 3}` and set `{2, 3, 4}` is the set `{1, 2, 3, 4}`.
Or formally: `{1, 2, 3} ∪ {2, 3, 4} = {1, 2, 3, 4}`


## Cartesian Product

The Cartesian product operator, defined by the production [`TYPE_EXPRESSION_PRODUCT`](../syntax/grammar.md#types), is defined as:


For sets A and B the Cartesian product between them is the set of ordered-pairs (a, b) where a is from A and b is from B.
Or formally: `A × B = {(a, b) | a ∈ A ∧ b ∈ B}`.

For example, the Cartesian product between the set `{1, 2}` and `{2, 3}` is the set: `{(1, 2), (1, 3), (2, 2), (2, 3)}`.
Or formally: `{1, 2} × {2, 3} = {(1, 2), (1, 3), (2, 2), (2, 3)}`.

## Set Difference

The set difference operator, defined by the production [`TYPE_EXPRESSION_DIFFERENCE`](../syntax/grammar.md#types), is defined as:

For sets A and B the set difference between them is the set of elements that are in A but not in B.
Or formally: `A - B = {x | x ∈ A ∧ x ∉ B}`.

For example, the set difference between the sets `{1, 2, 3}` and `{2}` is the set `{1, 3}`.
Or formally: `{1, 2, 3} - {2} = {1, 3}`

## Intersection

The intersection operator, defined by the production [`TYPE_EXPRESSION_INTERSECTION`](../syntax/grammar.md#types), is defined as:

For sets A and B the intersection between them is the set of all elements of A that are in B.
Or formally: `A ∩ B = {x | x ∈ A ∧ x ∈ B}`.

For example, the intersection between the sets `{1, 2, 3}` and `{1, 3, 4}` is the set `{1, 3}`.
Or formally: `{1, 2, 3} ∩ {1, 3, 4} = {1, 3}`.

