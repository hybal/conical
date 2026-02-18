# Memory Slot Model

A memory slot is an abstract location in memory that has a number of attributes associated with it.
These attributes are:

1. [Minimum Size](#minimum-size)
2. [Actual Size](#actual-size)
3. [Layout](#layout)
4. [Value Set](#value-set)
5. [Capability Set](#capability-set)
6. [Associated Set](#associated-set)

## Attributes

### Minimum Size

The minimum size attribute is the exact number of bits required to represent all possibilities in the value set.
This size is guaranteed no matter the environment or compiler.

### Actual Size

The actual size attribute is the number of bits that are determined based on the target backend and the decisions made by the compiler.
This is generally padding added to follow alignment and byte-based rules.

### Layout

The layout attribute is how the various elements in the value set are represented in-memory. 
By default the layout is determined by the compiler, however, the programmer can specify a custom layout using the `bits` compiler directive.

There does exist a requirement on externally linked and exported functions that the layouts of all inputs and outputs must follow the specified ABI requirements.

### Value Set

The value set attribute is the [set](../sets/definition.md) of possible values that this memory slot can represent or contain.

### Capability Set

> **NOTE**:
> This section is still in active development

The capability set attribute is the [set](../sets/definition.md) of operations that can be applied to this memory slot.
It can contain any value that a normal set can, and as such can be extended to support filtering custom operations.

The semantics of how these capabilities can be added and removed are still being developed

### Associated Set

The associated set is the compile-time [set](../sets/definition.md) of values, generally functions, that are associated with this memory slot.
This set is described as compile-time because the values it contains do not exist within memory, rather they are lifted into the global scope.
