# Conical

Conical is a work-in-progress systems programming language that makes use of a new form of static resource management.

## Getting Started

The project is currently in the **very** early stages so don't assume things work :)

To build the project make sure that you have LLVM-19 installed and accessible as well as the zig 0.14.0 compiler. Then just run
```bash
$ zig build
```
to get an executable at ./zig-out/bin/conical
or you can just run:
```bash
$ zig build run
```
to run it directly in which case add `--` to the end to pass arguments

The conical compiler currently only takes a single argument for a single file and produces an object file from that as well as some debug information (like a reconstruction of the AST and the emitted LLVM IR)

Then to actually run the binary you will have to have a c compiler in which case you can just link it like
```bash
$ CC file.o -o file
```
to create the executable.

## Syntax Overview

Note that currently the majority of the implemented syntax is not supported by the backend (meaning it wont compile). As such if it gives the error that it reached unreachable code, it is unsupported.

Variables are declared using `let` and `mut` for immutable and mutable variables respectively.
They also take an optional type after the identifier
```conical
let a = 4;
let b: i32 = 128;
mut c = 1.4;
c += 12;
```
all of the common arithmetic and logic operators are supported.

Functions are declared like:
```conical
fn foo(arg1, arg2): (i32, i32) -> i32 {
    arg1 + arg2
}
```
where the types of the parameters are seperated from the parameters themselves and the return type is specified with the arrow (`->`) operator.
Also note that semicolons have semantic meaning, they essentially "discard" the result of the operation making the type unit (essentially equivalent to void) also blocks will evaluate to the last expression in them.

Also note that the type system is currently broken since I had to do some weird things to be able to test external functions + strings, since there currently is no type coercion.

