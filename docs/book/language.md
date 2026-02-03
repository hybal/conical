# Syntax Overview

> **Warning**
> This is a very in-depth overview of the _entire_ language (even the stuff that hasn't been implemented yet), it is **not** a tutorial.
> Mostly this is just a place for me to write down my ideas and plans for the language.
> In addition, everything in this document is subject to change at any time.

> **NOTE**
> Because the language is still **very** early in development, there is no standard library.
> As such, any references to standard library functions / identifiers or builtin identifiers are essentially placeholders

## Literals

Most common literals will be supported.

```conical
1234 // Normal integers
0b01101 // Binary
0XDEADbeef // Hex, note that case is ignored
0o01234567 // Octal
0.124543 // Floating Point
.123 // As long as there is one decimal point its a float
1. // This version may be removed for clarity reasons idk
0.1e23 // Scientific notation (literally just literal [eE] signed integer literal)
-1.0 // signs are of course supported, including +
+2
1_000_12 // all number literals support underscores as whitespace

'A' // character literal
'ᚹ' // Unicode is supported as well, note that character literals are polymorphic, meaning that their size is based on the data in them (since they are just desugared to numbers).

"Hello, World!" // A utf-8 encoded string, since strings are arrays they can't be polymorphic like characters, so instead they are byte-length (utf-8).

.abc // if the type allows it, arbitrary identifiers can be used as values, these are called symbols (the leading period may be removed)

```

## Variables

```conical
let a = 1; // an immutable variable
mut b = 2; // a mutable variable
b += 1;
let a: i32 = 51; // variables can have optional types, otherwised its inferred
let a = "Hello"; // variables can be rebound, in which case the previous variable is no longer accessible
```

## Functions

```conical
// There are two syntaxes for functions, postfix:
fn add(a, b): (i32, i32) -> i32 {
    a + b
}

// and infix:
fn add(a: i32, b: i32) -> i32 {
    a + b
}

//note that the two styles *cannot* be mixed

// Arguments can also have default values
fn add2(a = 1, b): (i32, i32) -> i32 {
    a + b
}

fn add2(a: i32 = 1, b) -> i32 {
    a + b
}

```

### Function Call

Function calls are pretty much standard, with a couple of additions.

```conical
add(1, 2) // -> 3
```

One such addition is that you can pass an argument by its name, which is also required to set default arguments:
```conical
add2(2) // -> 3
add2(a: 4, 2) // -> 6
add2(a: 4, b: 3) // -> 7
```
> **NOTE**
> The syntax to set a default may change, I am debating on using the `.id = value` or `id: value` syntax for type constructors, and I want everything to be uniform.

## Blocks

A block is a section of executable code that is scoped together, meaning anything declared in the block is local to that block.
Blocks can also evaluate to a value, which is done by leaving off the semicolon on an expression.

```conical
// functions require a block
fn main() {
    let a = 1;
    {
        let b = a + 4; // a is accessible since its a parent block
    }
    // b is no longer accessible here
    let b = {
        a * 4 + 12
    }; // b = 16
}
```
You can also return a value from a function using the block expression (hence why there wasn't a return keyword in the previous add function examples)

## Control Flow

Branching and loops are both supported in conical.

```conical
if 1 < 2 { // parenthesis are only required if you don't use a block
    // do stuff
} else if {
    // do more stuff
} else {
    //do even more stuff
}

let a = if (1 > 2) 1 else 2; // inline if statement can be used, and takes the place of a ternary operator

mut c = 1;
while true { // while loops work like expected
    c += 1;
}

// for loops only work on iterators rather than the C style
for i in range(1, 10) { // range(1, 10) is a range from 1 to 10, meaning i will go through every value from 1 to 10 inclusive
    // do stuff
}

```
Loops cannot yield values, and as such have the type unit `()`

### Types

The basic struct and enum syntaxes exist and can be used:
```conical
type Vec2 = struct {
    x: f32,
    y: f32
};

type Direction = enum {
    North,
    South,
    East,
    West,
    Exact: u16,
};
```
Where enums are algebraic data types and can have associated values

However, these things are just syntax sugar.

In conical, all types are treated as sets, these sets essentially describe the exact values a memory location with that type can hold.

For example, the most basic type is `unit` which represents a set with no values:
``` conical
type Unit = ();
```
(The parenthesis may be changed to `{}` or `.{}` depending on how compound type literal syntax evolves)

A bit of a more interesting type is:
```conical
type u8 = 0..255;
```
This is a type that can contain the integer values 0 to 255 inclusive.
The `..` syntax is the range operator, it is just a convenient way to do numeric ranges.
It is recommended to use ranges whenever possible, as it makes it faster for the type system to do type checking.

You can also of course just use one number like so:
```conical
type One = 1;
```
in which case there is only one possible value, 1.

```conical
let a: One = 2; // This will cause a type error.
```
You can use pretty much any literal value in a type, including strings and characters.

You can also use "symbols" which are just identifiers, these are very similar to the traditional C enum variants

Of course just having single literals, or ranges, is not very useful. 
So conical also supports common set _operations_.

These are: union (`|`), cartesian product (`*`), difference (`-`), and intersection (`&`).

There are also boolean operations that if evaluate to false will cause a compile-time error, and when true will just keep the left side.

These are, subset (`<=`), strict subset (`<`), superset (`>=`), strict superset (`>`), equality (`==`), and set inclusion (`in`).

## Union

A type union is equivalent to the algebraic enum from before, just that it doesn't require labels for each of the variants (in fact that is what the enum syntax desugars to).

## Product

A product is equivalent to a struct, it differs to a union in that it requires a label for each of its members

> **NOTE**
> Labels may or may not be required in the final version

## Difference and Intersection

These are as expected, except for the fact that any associated functions on the types being operated on are removed, as these are destructive operations and as such it is impossible (or at least infeasible) to prove that they would work with the existing functions.

## Generics

Generics in conical are only allowed in functions.
Which means that to implement generic data types, you have to wrap them in a compile-time function.
This is commonly done with a lambda on the type declaration:
```conical
type Option = \$T => T | null;
```

## Compile-time Execution

> **NOTE**
> The syntax for these features has not yet been finalized

Compile-time functions in conical are functions that when used are evaluated at compile-time.
As such, they cannot rely on runtime values (unless those values can be lifted to a compile-time context).
They can also return types.

> **NOTE**
> There may in the future also be compile-time blocks

Macros also run at compile-time. 
However, unlike compile-time functions they can return _code_ rather then just values.
They can also take-in code as parameters.
They also have the restriction that they must be pure, that is they cannot have any side-effects.
This doesn't mean that they can't rely on values produced via impure compile-time functions however.
This allows macros to be efficiently cached and evaluated.
This makes it so that the majority of compile-time functionality that is used is in compile-time functions which can be executed later in the process rather then being required to be executed after parsing.

Macros also take-in code via an opaque `Ast` type.
This type is **_not_** the internal AST, rather it is a type that can be queried and built using builtin compiler functions.

Macros are not limited to just taking in an AST type, rather they can take in actual values that the compiler then checks.

In the future there may also be access to the tokenizer and/or a raw code string.

All of this makes it so that there is no need for an external dependency to be able to parse conical code in a macro (which is a big problem in Rust), while also keeping things that are related to code transformation performant.


# Semantics

## Memory Slots

> **NOTE** This section will likely become out of date as the system evolves
> I will try to keep it up-to-date as much as possible.

All data in Conical is internally represented using the concept of a _memory slot_.
A memory slot is an abstract location in memory that data is stored, it has 4 different attributes: size, value set, meta set, and the associated-function set.

The size is just the number of bits that the memory slot is _required_ to have.
This does not mean that the slot _will_ be that size, just that it cannot be smaller then that.

The value set is analogous to other languages' types.
It is the set of values that this slot can store.
It could in-theory be generalized to be the set of _bit-patterns_ that this slot can have, but the "types" of values is retained to help with correctness and to be able to pass them to the backend (as for example, different backends support different floating point representations).

The meta set is a compile-time only set of usage constraints. 
It can hold arbitrary values, however the compiler has some builtin values that it automatically adds to types as it does semantic analysis.
For example, the `mutable` builtin is added when a slot gets mutated in any way. 
The compiler then checks if the inferred meta set is a subset of the expected one. 
If it isn't it is a semantic error.

Some planned meta builtin categories are:

- mutable 
- alias 
- move
- thread-local 
- etc.

In-addition to usage rules, the set will also contain lifetime information.
How this will be done is still being worked on.

The last attribute on memory slots is the set of functions that are associated with it.
This is a separate set to keep the meta-set distinct.
It also only exists at compile time, all of the functions are lifted to the global scope and all references to them are replaced with those functions.
This can also be used to add any value that only exists at compile time.

## Resource Management

Automatic resource management in Conical will be implemented using scope-based lifetime inference.
Unlike most lifetime-based languages (e.g. Rust), this approach does not seek to be granular by determining exactly where a value/slot is no longer used.
Rather, it merely determines the last scope it is used via escape-analysis and inserts clean-up code at the end of said scope.
This does not mean that it can't be more granular, instead, that is relegated to being an optimization rather then being the default.

