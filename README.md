# Rooster
A programming language for verified software.

## Testing
Type in the workspace directory:

    cargo run std::logic::Eq::symmetric

This will verify `Eq::symmetric` in `./std/logic.roo`.
To verify any theorem of yours, type it in a file
and access its path like this. You can import symbols
from `std` using the following syntax:

```rust
let Eq = super::std::logic::Eq;
let Eq::sym = super::std::logic::Eq::symmetric;
```

To use `annotate-snippets` for error reporting
instead of `ariadne` (the default), pass the
`--features annotate-snippets` and `--no-default-features`
flags to `cargo`.

## TODO
* More small changes around names.
  - Namespace-related warnings.
* Bugfix checkpoint.
  - Fix undefined identifiers soemtimes not generating an error.
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
* Checkpoint before further feature work.
  - Check all TODOs in code.
  - Enable some stricter clippy warnings.
  - Split large functions.
  - Split long source files.
  - Improve project structure (use cargo-modules).
* Improve performance.
  - Use string interning.
  - Avoid fully normalizing in parser unless necessary.
  - Implement comparison without full normalization.
