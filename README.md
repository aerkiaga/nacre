# Rooster
A programming language for verified software.

## TODO
* Perform more thorough error reporting.
  - Implement recovery mechanism through "ignore" AST variant.
  - Implement error reporting infrastructure at kernel level.
  - Make sure the `_ =>` in the parser are not accepting invalid input.
* Accept a simpler call syntax.
  - Allow omitting any preceding type parameters.
  - Allow omitting non-type preceding parameters.
  - Allow dot syntax for "self" function call.
* Improve logical foundation.
  - Allow axiomatization of inductive principles.
  - Consider separating `Prop` and `Set` for compatibility with proof irrelevance.
  - Consider allowing axiomatization of axiom K or equivalent.
* Check all TODOs in code.
