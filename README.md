# Rooster
A programming language for verified software.

## TODO
* Checkpoint before further feature work.
  - Fix Rust warnings.
  - Explore alternate compiler diagnostics crates.
  - Explore clippy usage.
* Accept a simpler call syntax.
  - Allow omitting any preceding type parameters.
  - Allow omitting non-type preceding parameters.
  - Allow dot syntax for "self" function call.
* More small changes around names.
  - Variable naming-related warnings.
  - Namespace-related warnings.
  - Private path implementation.
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
* Check all TODOs in code.
