# Conical Standard Library

> **WARNING** 
> Conical is not yet in a useable state.
> Everything in this directory is unstable and can change at any time, and is not guaranteed to compile on current versions.

The standard library for conical is organized into three main sections: core, std, and extra.

Core contains all declarations required for general use of the language, everything in it must be able to exist on all supported targets (within reason). 
This does not mean that things that require target-specific functionality cannot exist in core, only that they must be able to support all targets.

Std contains declarations that are very commonly used in production.

Extra contains everything that doesn't fit in core/std, except bindings which are not allowed except in the case of OS APIs or syscalls.

`prelude.cnl` is also included at the root of this directory and contains things from core that are automatically included in every program.

# Style Guidelines

Single-file modules should be singular and have a descriptive single-word name to refer to their contents.
Multi-file modules should be put into a subdirectory that follows the same naming rules as single-files. In addition, the root of the module should be in a file called `mod.cnl` in the root of the module directory, and only contain re-exports of declarations that are intended to be included when importing the module as a whole. All other declarations should be in separate files/folders following the same rules and prefixed with the relevant module name.
