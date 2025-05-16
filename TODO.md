# Todo

NOTE: this is in no particular order

- [x] return statements
- [ ] break/continue (with labels)
- [ ] labeled blocks
- [ ] type literal constructors (syntax and implementation)
- [ ] finish type coercion
- [x] access operator (.)
- [ ] fix diagnostic messages (the spans are fine, just the messages)
- [x] add checks to make sure that user types are actually valid/defined
- [ ] reorganize code / general cleanup
- [ ] make universal function for creating "compilation units" instead of just calling piecemeal
- [ ] decide whether or not to use semicolons in struct type declarations
- [ ] decide on the syntax for tagged unions / sum types (either `tag: type` or `tag(type)` im leaning towards the first one)
- [ ] decide on the syntax for enum literals + implement it
- [ ] add generics
- [ ] add impl blocks
- [ ] work on how the interface system should work
- [ ] add lifetimes + memory stuff (will have to come after interfaces/traits are done)
- [ ] documentation
- [ ] type inference
- [ ] decide module syntax + semantics
- [ ] LLVM backend (must be after MIR is done)
- [ ] decide on MIR structure (if SSA what level?, or else maybe System F (unlikely since it is still technically imperative) or CPS or even CPS+SSA)
- [ ] implement MIR
- [ ] expand syntax sugar (during parsing and/or semantic analysis)
- [ ] finalize type constraint syntax + interface (what to return, what is passed, etc.)
- [ ] implement type constraints
- [ ] add command line interface + be able to compile multiple compilation units
