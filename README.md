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

# fuzz testing
```shell
cargo afl build --example fuzz_consistency
cargo afl fuzz -i nacre_kernel/examples/in -o nacre_kernel/examples/out target/debug/examples/fuzz_consistency
```

## TODO
* Checkpoint before further feature work.
  - Split large functions.
  - Split long source files.
* Improve performance.
  - Use string interning.
  - Avoid fully normalizing in parser unless necessary.
  - Implement comparison without full normalization.
  - Parallelize verification at AST conversion level.
* Add tests.
  - Parser fuzz test.
  - Regular test suite.
  - Mutation testing.
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
