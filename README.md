# systemf-scala

Implementation of type inference for System F in Scala 3.
Based on https://github.com/AndrasKovacs/elaboration-zoo .

## Goals

- Optional explicit type abstraction and application
- Kinds and kind inference
- Type aliases
- Algebraic datatypes
- First-class polymorphism (aka impredicative instantiation)

## Limitations
- No let-generalisation, except for on the top-level
- No subtyping or subsumption

## TODO

- [x] Kind metas
- [x] Top-level generalization
- [x] Definitions
- [x] Type aliases
- [ ] Mutual recursion
