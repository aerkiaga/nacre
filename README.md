# Nacre
A programming language for verified software.

## Usage
Type in the workspace directory:

```shell
cd nacre
cargo run std::logic::Eq::symmetric
```

This will verify `Eq::symmetric` in `./std/logic.na`.
To verify any theorem of yours, type it in a file
and access its path like this. You can import symbols
from `std` using the following syntax:

```rust
use super::std::logic::Eq;
```

To use `annotate-snippets` for error reporting
instead of `ariadne` (the default), pass the
`--features annotate-snippets` and `--no-default-features`
flags to `cargo`.

## Testing
```shell
cargo check         # build
cargo clippy        # linter
cargo test --tests  # integration tests
cargo llvm-cov      # test coverage
cargo bench         # performance regressions
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

* Add tests.
  - Parser fuzz test.
  - Regular test suite.
  - Mutation testing.
* Finish core syntax.
  - Implement named parameters in function application.
  - Implement struct and enum definition syntax.
  - Add support for more kinds of lvalues.
* Implement barebones code generation.
  - Write code to convert CoC into closures.
  - Write code to optimize closures into static code
  	and data structures.
  - Add at least a few built-in types.
  - Investigate and use LLVM.

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
