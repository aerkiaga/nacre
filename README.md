# Nacre
A programming language for verified software.

## Usage
Type in the workspace directory:

```shell
cargo run nacre::std::logic::Eq::symmetric
```

This will verify `Eq::symmetric` in `./nacre/std/logic.na`.
To verify any theorem of yours, type it in a file
and access its path like this. You can import symbols
from `std` using the following syntax:

```rust
use super::nacre::std::logic::Eq;
```

To use `annotate-snippets` for error reporting
instead of `ariadne` (the default), pass the
`--features annotate-snippets` and `--no-default-features`
flags to `cargo`.

At the moment, code generation is very
basic and the ABI is not complete.
There is an integration test for compilation,
which emits an object file and links it
against a test program, which is then run.

## Testing
```shell
cargo check         # build
cargo clippy        # linter
cargo test          # integration tests
cargo llvm-cov      # test coverage
cargo bench         # performance regressions
cargo mutants --test-workspace true       # mutation testing
```

### Fuzz testing
```shell
cargo afl build --example fuzz_consistency
cargo afl fuzz -i nacre_kernel/examples/in -o nacre_kernel/examples/out target/debug/examples/fuzz_consistency
```

## Roadmap
### Early bootstrapping phase
Currently, a compiler is being developed in Rust,
but the roadmap is focused on eventual self-hosting.
This means that this initial development will focus
on _correctness_ and _core_ features, but not
necessarily performance or even code quality
(although readable code is desired, so as to serve
as reference for later development).

* Complete and stabilize code generation.
  - Structs and enum variants with contents.
  - Built-in operators.
  - Array and integer types.
  - Generic types (and related CoC-level transformations).
  - Partial application.
  - Non-de-closureable usages.
* Finish core syntax.
  - Implement named parameters in function application.
  - Implement struct and enum definition syntax.
  - Add support for more kinds of lvalues.
  - Add core operators.
* Add tests.
  - Parser fuzz test.
  - Regular test suite.
  - Mutation testing.

### Research phase
Once the bootstrapping compiler is capable of
generating actual (albeit slow) code, the next
step would be to carefully expand its capabilities
and the standard library, working towards the
implementation of a self-hosting compiler.

* Implement macros.
  - Design the macro system.
  - Get JIT compilation ready.
  - Get basic macros to a working state.
* Improve logical foundation.
  - Allow axioms from allowlisted paths.
  - Explore if a generalized induction axiom can be
    expressed and used in a reasonable way.
  - Explore whether axiomatization of type-specific
    indefinite description is consistent and sufficient
    to prove the types' uniqueness.
  - Investigate what types should be given this
    axiomatization at minimum (e.g. equality).
  - Write axioms.
* Extend the standard library.
* Implement low-hanging optimizations.

### Self-hosting compiler development
Having brought the compiler and language to
a reasonable degree of maturity, the most critical
task begins: writing a self-hosting compiler
with all the desired features, performance
and optimization capabilities.
