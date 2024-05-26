# Rooster
A programming language for verified software.

```
/* Compile me with 'rooster build example::main' */
#global // this macro rewrites main() to add global state
fn main() {
    #println("Hello, world!");
}

fn wrong() {
    This definition contains very obviously invalid syntax,
    and yet the compiler will ignore it when compiling main().
}
```

```
fn triple(x: u32) -> u32 {
    (x << 1) + x
}

impl triple {
    fn correct(x: u32) -> triple x === 3 * x {
        // first prove that ∀x: u32, x << 1 = 2 * x
        let shift_is_double = u32::shl::power_of_two 1;
        // then add x to both sides in the equation
        // u32 impl's std::arith::Modular,
        // which impl's std::arith::Ring
        {
            let * = std::algebra::RingEquation::*;
            add_right_both shift_is_double x
        }
        // verification automatically computes both sides, so
        // ∀x: u32, (x << 1) + x = 2 * x + x is our theorem:
        // QED
    }
}
```

## Design
These items are planned design features or traits:
- Equivalent to pure CoC.
- Functional-style calls and partial application.
- Fully multithreaded compiler/interpreter.
- Basic compilation unit is top-level definition.
- Everything allowed at top-level is allowed in nested scopes and vice-versa.
- Code-rewriting macro system.
- Most language features defined in standard library:
  * Enums, structs and traits (as macros).
  * Basic types (as derived types).
- Flexible grammar:
  * Numbers are valid identifiers, use special member function when undefined.
  * Most operators evaluate to overrideable function calls.
  * Wildcard assignment for library imports.
- Standard library compiled ad-hoc rather than linked.
- Optimization at whole binary level.

## Progress
Currently only the lexer and first-stage parser are implemented.
