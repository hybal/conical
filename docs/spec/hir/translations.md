# Ast to Hir Translations

> **NOTE**:
> This document is provisional, it has not yet been formalized.

## Loops

For a while loop defined as:

```conical
while cond {
    expressions*
}
```

It should be translated as such:

```conical
loop {
    {
        expressions*
    }
    if (!cond) break;
}
```

For a for loop defined as:

```conical
for id in iter {
    expressions*
}
```
It should be translated as such:

```conical
loop {
    let id = iter.next();
    if (id == .null) break;
    {
        expressions*
    }
}
```
> **NOTE**:
> The actual stop iteration value will likely change.

## Types

For a struct syntax sugar expression defined as:

```conical
struct {
    a: type0,
    b: type1,
    ...
}
```

It should be translated thus:
```conical
(a: (type0)) * (b: (type1)) * (...)
```

For an enum syntax sugar expression defined as:

```conical
enum {
    literal_a,
    literal_b,
    ...
}
```

It should be translated as:
```conical
(literal_a) | (literal_b) | (...)
```

For an impl syntax sugar expression defined as:

```conical
impl {
    fn a(a, b, ...): (A, B, ...) -> R {
        expression*
    }
    ...
}
```

It should be translated as:
```conical
(a: fn (A, B, ...) -> R = \(a, b, ...) => { expression* }) * (...)
```

## Compound Assignment Operators

Given the compound assignment `a op= b`, the expression should be translated to `a = a op b`. 

## Operators

Given a generic binary operator `op`, in the expression `a op b` and the corresponding operator overload function `overload(op)` the expression should be translated to: `a.overload(op)(b)`.

Given a generic unary prefix operator `op`, in the expression `op a`, it should be translated to `a.overload(op)(a)`. Note that the unary prefix operators `&`, `*` are not included in this translation step.

Given the postfix operator `!`, in the expression `a!` it should be translated to
```conical
match a.overload(!)(a) {
    .{ left: _ } => |l| return l.left,
    .{ right: _ } => |r| r.right,
}
```

Given the postfix indexing operator in the expression `a[i]` it should be translated to:
```conical
a.overload([])(a, i)
```

### Sequence Operator

Given the binary operator `|>` in the expression `a |> b`, it should be translated to:
```conical
b(a)
```

## Refinements

Given an expression like:
```conical
if cond |a| {

}
```
It should be translated to:
```conical
if cond |a=a| {

}
```


