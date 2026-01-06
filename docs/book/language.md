# Language Overview

> **Warning**
> This is a very in-depth overview of the _entire_ language (even the stuff that hasn't been implemented yet), it is **not** a tutorial.
> Mostly this is just a place for me to write down my ideas and plans for the language.

# Literals

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

abc // if the type allows it, arbitrary identifiers can be used as values, these are called symbols
```

# Variables

```conical
let a = 1; // an immutable variable
mut b = 2; // a mutable variable
b += 1;
let a: i32 = 51; // variables can have optional types, otherwised its inferred
let a = "Hello"; // variables can be rebound, in which case the previous variable is no longer accessible
```

# Functions

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

## Function Call

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
> The syntax to set a default may change, I am debating on using the `.id = value` or `id: value syntax` for type constructors and I want everything to be uniform

# Blocks

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

# Control Flow

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

## Types

Types are where conical gets its strength.

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
    West
};
```
Where enums are algebraic data types and can have associated values

However, conical does not stop at these rather these things are just syntax sugar.

In conical, all types are treated as sets, these sets essentially describe the exact values a piece of data with that type can hold.

For example, the most basic type is `unit` which represents a set with no values:
``` conical
type Unit = ();
```
(It uses parenthesis instead of brackets since unit can also be used as a value in normal code)

A bit of a more interesting type is:
```conical
type u8 = 0..255;
```
This is a type that can contain the integer values 0 to 255 inclusive.
the `..` syntax is the range operator, it is just a convenient way to do numeric ranges.

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

# Union

A type union is equivalent to the algebraic enum from before, just that it doesn't require labels for each of the variants (in fact that is what the enum syntax desugars to).

# Product

A product is equivalent to a struct, it differs to a union in that it requires a label for each of its members

> **NOTE**
> Labels may or may not be required in the final version

# Difference and Intersection

These are as expected, except for the fact that any associated functions on the types being operated on are removed, as these are destructive operations and as such it is impossible (or at least infeasible) to prove that they would work with the existing functions.

# Generics

Generics in conical are only allowed in functions.
Which means that to implement generic data types, you have to wrap them in a compile-time function.
This is commonly done with a lambda on the type declaration:
```conical
type Option = \$T => T | null;
```







