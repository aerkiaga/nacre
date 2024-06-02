# Rooster
A programming language for verified software.

## TODO
* Accept more conventional syntax for current constructs.
  - Implement explicit declaration types.
  - Allow trailing semicolons.
  - Allow trailing commas.
* Perform more thorough error reporting.
  - Implement recovery mechanism through "ignore" AST variant.
  - Replace all parser panics with error, unreachable or todo.
* Accept a simpler call syntax.
  - Allow omitting any preceding type parameters.
  - Allow omitting non-type preceding parameters.
  - Allow dot syntax for "self" function call.
