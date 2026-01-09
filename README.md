# Romanesco (Reduced)

A minimal, typeless compiled language backed by the Z3 SMT solver.

This project was originally an ambitious attempt at a universal, self-modifying language. It has been drastically reduced to its core value proposition: using an SMT solver for execution and safety verification.

## Features

- **Z3 Solver Integration:** Variables and expressions are directly mapped to Z3 constraints.
- **Safety:** The solver verifies the feasibility of your code.
- **Simplicity:** No macros, no complex optimizations. Just parsing and solving.

## Requirements

- Scala 3.7.4 (or compatible)
- sbt 1.12.0 (or compatible)
- Z3 installed and accessible (via `z3-turnkey` dependency)

## Usage

Run the compiler with a source file:

```bash
sbt "run <file>"
```

## Example

```
x = 10;
y = 20;
z = 30;
x + y == z
```

Output:
```
Solver Status: SATISFIABLE
x = 10
y = 20
z = 30
```