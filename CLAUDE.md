# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Romanesco is a multi-logic theorem prover written in Scala 3 (3.8.1) with no external library dependencies. It uses a plugin-based architecture to support category theory, linear logic, Hoare logic, temporal logic, modal logic, and Homotopy Type Theory (HoTT). The project language (comments, docs, commits) is Japanese.

## Build & Run Commands

```bash
# Build
sbt compile

# Run all tests (PowerShell)
.\run.ps1

# Run a single test
sbt "runMain romanesco.Solver.SolverTests.HoTTTest"

# Interactive REPL
.\run.ps1 repl
# or: sbt "runMain romanesco.Solver.runRepl"
```

All tests are `@main` methods under `romanesco.Solver.SolverTests.*` (not a test framework). Each test file runs as a standalone program.

## Architecture

**Source layout:** All Scala source is under `scala/` (configured in build.sbt as `Compile / scalaSource`).

### Core components (`scala/Solver/`)

- **Core.scala** — Central data structures: `Expr` (AST with `Var`, `Meta`, `Sym`, `App`), `MetaId`, `SearchNode`, `LogicState`, `Goal` (persistent context + linear context + goal expr), `CatRule` (categorical morphism rules).
- **Prover.scala** — Proof search engine using iterative deepening (depth-limited BFS from 1 to maxDepth). Returns `Tree[SearchNode]`. Features cycle detection, divergence detection, failure caching (subset-aware), lemma caching, and timeout/deadline enforcement.
- **Unifier.scala** — Meta-variable unification with universe level checking (prevents Russell's paradox) and lambda-abstraction unification.
- **Rewriter.scala** — Term rewriting with AC (associative-commutative) normalization, idempotency, and short-circuit evaluation.

### Plugin system

All logic systems are plugins implementing the `LogicPlugin` trait, which hooks into search via `getGoalHooks()` and `getContextHooks()`.

- **LogicPlugin.scala** — Plugin interface definition.
- **StandardPlugins.scala** — Registers built-in plugins: `AxiomPlugin`, `IntroductionPlugin`, `LinearLogicPlugin`, `TemporalLogicPlugin`, `PersistentLogicPlugin`, `HoTTPlugin`, `HoareLogicPlugin`, `UserRulePlugin`, `InductionPlugin`, `RewritePlugin`.
- **StandardRules.scala** — ~80 inference rules using categorical morphisms (∘, pair, pi1, pi2, case, λ).
- **LinearLogicSearch.scala** — Linear/separation logic with frame inference.
- **HoareLogicSearch.scala** — Program verification (skip, assign, seq, if, while).
- **HoTTSearch.scala** — Path induction, higher inductive types.
- **ModalLogicSearch.scala** — Possible/necessary worlds.
- **TemporalLogicSearch.scala** — Globally, Finally, Until operators.

### Other modules

- **Parser/** — Tokenizer → ParseTree pipeline with modifiable syntax/lexical rules (`rParser.scala`, `Tokenizer.scala`, `ParseRules.scala`).
- **Types/Types.scala** — Generic `Tree[T]` data structure used throughout.
- **Utils/** — Debug logging, macro system, file I/O, timing.
- **Repl.scala** — Interactive tactic-based proof shell with undo/abort.
- **visualizer/index.html** — D3.js-based search tree visualizer (web).

## Key Design Patterns

- **Linear context tracking**: Goals carry both a persistent context and a separate linear context for resource-sensitive logics.
- **Universe hierarchy**: Strict universe levels on types to prevent paradoxes.
- **Category theory foundation**: Inference rules are categorical morphisms; the Curry-Howard-Lambek correspondence is central.
- **Tree-based search results**: Full `Tree[SearchNode]` returned for debugging and visualization.
