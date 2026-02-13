# Romanesco Logic Engine Manual

Romanesco is a universal automated theorem prover supporting multiple advanced logic systems, including Classical, Modal, Linear, Temporal, Separation Logic, and Homotopy Type Theory (HoTT).

## 1. Supported Logics & Syntax

Romanesco parses a unified syntax where different logic operators can coexist.

### Classical & Intuitionistic Logic
| Concept | Symbol | ASCII Alt | Example |
|---|---|---|---|
| Implication | `→` | `->` | `A → B` |
| Conjunction | `∧` | `&` | `A ∧ B` |
| Disjunction | `∨` | `|` | `A ∨ B` |
| Negation | `¬` | `~` | `¬A` |
| Forall | `∀` | `forall` | `∀x. P(x)` |
| Exists | `∃` | `exists` | `∃x. P(x)` |
| Equality | `=` | `=` | `x = y` |

### Modal Logic (S5)
| Concept | Symbol | ASCII Alt | Example |
|---|---|---|---|
| Box (Necessity) | `□` | `Box` | `□A` |
| Diamond (Possibility) | `◇` | `Diamond` | `◇A` |

### Linear Logic
| Concept | Symbol | ASCII Alt | Example |
|---|---|---|---|
| Linear Implication | `⊸` | `-o` | `A ⊸ B` |
| Tensor Product | `⊗` | `*` | `A ⊗ B` |
| Bang (Of Course) | `!` | `!` | `!A` |

### Temporal Logic (LTL)
| Concept | Symbol | ASCII Alt | Example |
|---|---|---|---|
| Globally (Always) | `G` | `G` | `G(A → B)` |
| Finally (Eventually)| `F` | `F` | `F(A)` |
| Next | `X` | `X` | `X(A)` |
| Until | `U` | `U` | `A U B` |

### Separation Logic
| Concept | Symbol | ASCII Alt | Example |
|---|---|---|---|
| Separating Conjunction | `*` | `*` | `(x ↦ v) * (y ↦ w)` |
| Points To | `↦` | `|->` | `x ↦ v` |

### Homotopy Type Theory (HoTT)
| Concept | Symbol | Example |
|---|---|---|
| Path Type | `path` | `path(A, x, y)` (equivalent to `x = y`) |
| Univalence | `equiv` | `equiv(A, B) → x = y` |

---

## 2. Tactics System

When using the Interactive Shell (`run.ps1 repl`), you can use the following tactics to guide the proof.

- **`auto`**: Automatically attempts to prove the current goal using all available rules and context. It is context-aware and handles deep search.
- **`simpl`**: Normalizes the goal and hypotheses (reduces definitions like `plus`, `append`, `map`).
- **`intro [name]`**: Moves antecedents from the goal into the context (e.g., `A → B` becomes goal `B` with hypothesis `A`).
- **`induction [var]`**: Performs structural induction on a variable (e.g., `n` for Nat, `xs` for List). Romanesco automatically selects the correct algebra based on the variable name prefix.
- **`rewrite <hyp>`**: Rewrites the goal using an equality hypothesis `hyp`.
- **`apply <hyp>`**: Applies an implication hypothesis `hyp` (e.g., if `H: A → B` and goal is `B`, change goal to `A`).
- **`destruct <hyp>`**: Breaks down a hypothesis (e.g., `A ∧ B` becomes `A` and `B`).
- **`split`**: Splits a conjunction goal `A ∧ B` into two subgoals `A` and `B`.

---

## 3. Standard Library (Algebras & Rules)

Romanesco comes with built-in support for common data structures.

### Natural Numbers (`Nat`)
- **Constructors**: `0`, `S(n)`
- **Variables**: `n`, `m`, `k`, `i`, `j`
- **Operations**:
    - `plus(n, m)`: Addition (Commutative, Associative)
    - `times(n, m)`: Multiplication (Commutative, Associative, Distributive)
    - `exp(b, n)`, `fact(n)`, `gcd(n, m)`, `lcm(n, m)`

### Lists (`List`)
- **Constructors**: `nil`, `cons(x, xs)`
- **Variables**: `xs`, `ys`, `zs`, `l`, `m`
- **Operations**:
    - `append(xs, ys)`: Concatenation (Associative)
    - `length(xs)`: List length
    - `map(f, xs)`: Functor mapping
    - `return(x)`, `bind(m, f)`: Monad operations

### Other Structures
- **Tree**: Binary tree (`leaf`, `node`).
- **Vec**: Dependent vector (`vnil`, `vcons`).
- **Stream**: Infinite stream (`cons_stream`). Use `guarded` co-induction for proofs.

---

## 4. Advanced Features

- **Co-induction**: Romanesco automatically detects cycles in proofs guarded by constructors (e.g., `bisim` for streams) and accepts them as valid co-inductive proofs.
- **Frame Rule**: In Separation Logic, the engine automatically cancels out common heap resources on both sides of an implication (`A * C ⊸ B * C` becomes `A ⊸ B`).
- **Path Induction**: In HoTT, the J-rule is automatically applied to reduce paths based on their endpoints.
