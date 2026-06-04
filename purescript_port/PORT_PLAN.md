# PureScript Compiler — Port to PureScript (Self-Hosting)

## Goal

Port the PureScript compiler from Haskell to PureScript itself so that:

1. The compiler is **self-hosting** — it can compile its own source.
2. The compiler can be **compiled to JavaScript** (via the existing Haskell compiler), then run in the browser to power a **browser-based REPL**.

The resulting browser bundle will accept PureScript source text and produce JavaScript, with no server round-trip required.

---

## Scope

### In scope (browser REPL minimum)
- CST: Lexer, layout algorithm, parser, CST→AST conversion
- Sugar: all desugaring passes
- Type checker: kinds, types, unification, entailment, deriving, roles
- CoreFn lowering + laziness analysis + CSE
- CoreImp + JS codegen + printer
- Externs (module interfaces, switched from CBOR to JSON)
- In-memory single-threaded Make (no parallelism, virtual FS)
- Interactive REPL loop

### Out of scope (defer)
- `Docs/`, `Publish/` — documentation generation
- `Bundle.hs` — JS bundling (not needed in-browser)
- `Ide/` — language server / psc-ide
- `Make/BuildPlan.hs` parallelism — single-threaded is fine in browser
- `Linter/` — useful but not blocking

---

## Codebase Size Reference

| Stage | Haskell files | LOC | Priority |
|-------|--------------|-----|----------|
| Core types (Names, Types, AST, Errors) | 18 | ~8,270 | Phase 1 |
| CST (lex/layout/parse/convert) | 12 | ~4,031 | Phase 2 |
| Sugar | 19 | ~4,031 | Phase 3 |
| TypeChecker | 13 | ~6,298 | Phase 4 |
| CoreFn | 12 | ~2,231 | Phase 5 |
| CoreImp + CodeGen | 12 | ~2,200 | Phase 5 |
| Make (simplified) | 4 | ~1,007 | Phase 6 |
| Interactive / REPL | ~5 | ~700 | Phase 7 |
| **Total in-scope** | | **~28,768** | |

---

## Haskell → PureScript: Key Translation Challenges

### 1. Serialization format: CBOR → JSON
Externs files (`.cbor`) use `codec-serialise` / `DeriveAnyClass Serialise` from Haskell.
PureScript has no CBOR library. **Switch externs to JSON** using `codec-argonaut`.
The JSON format already exists in `CoreFn/ToJSON.hs` / `FromJSON.hs` — use it as the model.

### 2. `Generic` / `NFData` → drop
Haskell uses `GHC.Generics` for deriving and `deepseq`/`NFData` for strictness.
PureScript has no `Generic`-based deriving for serialization; write `codec-argonaut` codecs manually.
Drop `NFData` entirely — irrelevant in PS/JS.

### 3. `Data.Text` → `String` or `Data.String.CodeUnits`
The Haskell compiler uses `Text` throughout. PureScript's native `String` is UTF-16 code units.
For the lexer, use `Data.String.CodeUnits` for O(1) character access where needed,
or `Data.CodePoint.Unicode` for Unicode-correct code point iteration.

### 4. Mutable state: `IORef`/`STRef` → `Effect.Ref`
`IORef` maps directly to `Effect.Ref` in PureScript. Usage is confined to `Make/`,
`Ide/`, and the optimizer's `IORef`-based fresh name supply.

### 5. Concurrency: `MVar`/`async`/`forkIO` → eliminate
`Make/BuildPlan.hs` uses `async`/`MVar` for parallel module compilation.
For browser use, replace with a sequential pure build loop — no parallelism needed.

### 6. Pretty printing: `Text.PrettyPrint.Boxes` → `purescript-dodo-printer`
The Haskell code uses `boxes` for error messages and type pretty-printing.
Port to [`purescript-dodo-printer`](https://github.com/natefaubion/purescript-dodo-printer) (idiomatic PS pretty printing library).

### 7. File I/O: `System.Directory`/`filepath`/`glob` → virtual FS FFI
The browser has no native filesystem. Provide a `MakeActions` implementation backed by
a pure in-memory `Map String String` (path → content). Expose this as FFI so the browser
JS layer can seed it with preloaded Prelude modules.

### 8. Number literals: `scientific` → `Number`
Haskell uses `Data.Scientific` for arbitrary-precision numeric literals.
PureScript/JS uses `Number` (IEEE 754 double). This is a semantic simplification acceptable
for a browser REPL (same precision as the runtime anyway).

### 9. `Data.DList` → `Array` (with builder) or `List`
Difference lists (`DList`) are used for efficient appending in error accumulation.
Use `Array` with `(<>)` or an `Array.Builder` pattern in PS.

### 10. Happy parser (.y) → recursive descent or parser combinators
`CST/Parser.y` is an 818-line Happy (LALR(1)) grammar. PureScript has no Happy equivalent.
**Recommended approach**: Port to recursive-descent style using `purescript-parsing` (Parsec-style).
The grammar is unambiguous and already factored into named productions — mechanical translation is feasible.
Alternative: hand-write a CPS-based recursive descent parser matching the existing `ParserM` monad structure.

---

## Library Mapping

| Haskell library | PureScript equivalent |
|----------------|----------------------|
| `containers` (`Data.Map.Strict`, `Data.Set`) | `purescript-ordered-collections` |
| `Data.List.NonEmpty` | `purescript-nonempty` / `Data.List.NonEmpty` |
| `transformers` (`StateT`, `ExceptT`, `WriterT`) | `purescript-transformers` |
| `mtl` (`MonadState`, `MonadError`, `MonadWriter`) | `purescript-transformers` |
| `aeson` | `purescript-argonaut-codecs` + `purescript-codec-argonaut` |
| `Text.PrettyPrint.Boxes` | `purescript-dodo-printer` |
| `Data.Text` | `Data.String` / `Data.String.CodeUnits` |
| `Data.Char` | `Data.Char` / `Data.CodePoint.Unicode` |
| `Data.DList` | `Array` or `List` with difference-list encoding |
| `scientific` | `Number` |
| `codec-serialise` (CBOR) | `purescript-codec-argonaut` (JSON) |
| `Control.DeepSeq` | drop entirely |
| `GHC.Generics` | manual codec instances |
| `System.FilePath` | `Node.Path` (Node) or inline FFI (browser) |
| `System.Directory` | virtual FS via `Map` + FFI |
| `Data.IORef` | `Effect.Ref` |
| `Data.Map.Strict qualified as M` | `Data.Map as M` |

---

## Phased Implementation Plan

### Phase 0 — Scaffolding (Weeks 1–2)

**Deliverables:**
- `purescript_port/spago.yaml` — spago project targeting PureScript 0.15.x
- `purescript_port/src/` — source tree mirroring `src/Language/PureScript/` structure
- `purescript_port/test/` — test harness that runs golden `.purs` files from `tests/purs/passing/`
- CI: `spago build` passes on empty stubs

**Key decisions:**
- Target PureScript 0.15.16 (current stable, matches this repo)
- Use `spago` as build tool
- Use `purescript-parsing` for the parser port

---

### Phase 1 — Core Data Types (Weeks 2–4)

Port the foundational ADTs that every other module depends on.
These are mostly `data` declarations with `deriving (Show, Eq, Ord)` — mechanical to port.

**Files to create** (mirroring Haskell source):

| PureScript file | Haskell source | Notes |
|----------------|---------------|-------|
| `src/Language/PureScript/Names.purs` | `Names.hs` (322 LOC) | Ident, ProperName, Qualified, ModuleName, QualifiedBy |
| `src/Language/PureScript/PSString.purs` | `PSString.hs` (240 LOC) | PSString type, UTF-16 encoding helpers |
| `src/Language/PureScript/Label.purs` | `Label.hs` | row label type |
| `src/Language/PureScript/Roles.purs` | `Roles.hs` | type role declarations |
| `src/Language/PureScript/Comments.purs` | `Comments.hs` | Comment type |
| `src/Language/PureScript/Types.purs` | `Types.hs` (874 LOC) | Type ADT, SourceType, Constraint, traversals |
| `src/Language/PureScript/AST/SourcePos.purs` | `AST/SourcePos.hs` | SourceSpan, SourcePos, SourceAnn |
| `src/Language/PureScript/AST/Literals.purs` | `AST/Literals.hs` | Literal ADT |
| `src/Language/PureScript/AST/Binders.purs` | `AST/Binders.hs` | Binder ADT |
| `src/Language/PureScript/AST/Declarations.purs` | `AST/Declarations.hs` (868 LOC) | Declaration, Expr, Guard, etc. |
| `src/Language/PureScript/AST/Traversals.purs` | `AST/Traversals.hs` (721 LOC) | everywhereOn*, accumulate* |
| `src/Language/PureScript/CST/Types.purs` | `CST/Types.hs` (440 LOC) | Token, SourceToken, CST tree |
| `src/Language/PureScript/TypeClassDictionaries.purs` | `TypeClassDictionaries.hs` | TypeClassDictionaryInScope |
| `src/Language/PureScript/Errors.purs` | `Errors.hs` (2078 LOC) | SimpleErrorMessage, MultipleErrors, prettyPrint |

**Notes:**
- `Errors.hs` (largest single file, 2078 lines) contains the error ADT and all pretty-printing.
  Port the pretty-printing using `purescript-dodo-printer` instead of `boxes`.
- `Types.hs` uses many type-level tricks (`ProperNameType` phantom types) — these port cleanly to PS phantom types.
- Replace all `Serialise` instances with `codec-argonaut` codecs (write separately in `*Codec.purs` files to keep types clean).

---

### Phase 2 — CST Frontend (Weeks 4–8)

Port the lexer, layout algorithm, and parser.

| PureScript file | Haskell source | Notes |
|----------------|---------------|-------|
| `src/.../CST/Errors.purs` | `CST/Errors.hs` (201 LOC) | ParserError, ParserErrorType |
| `src/.../CST/Positions.purs` | `CST/Positions.hs` (338 LOC) | advanceLeading, textDelta, etc. |
| `src/.../CST/Monad.purs` | `CST/Monad.hs` (187 LOC) | ParserM (CPS StateT), LexState |
| `src/.../CST/Layout.purs` | `CST/Layout.hs` (552 LOC) | Layout rule insertion (indent-sensitive) |
| `src/.../CST/Lexer.purs` | `CST/Lexer.hs` (780 LOC) | Character-level tokenizer |
| `src/.../CST/Parser.purs` | `CST/Parser.y` (818 LOC) | **See note below** |
| `src/.../CST/Utils.purs` | `CST/Utils.hs` (360 LOC) | Helper combinators |
| `src/.../CST/Flatten.purs` | `CST/Flatten.hs` (315 LOC) | Flatten separated lists |
| `src/.../CST/Convert.purs` | `CST/Convert.hs` (710 LOC) | CST → AST lowering |
| `src/.../CST/Print.purs` | `CST/Print.hs` (96 LOC) | CST pretty-printer |

**Parser strategy:**
The Haskell parser is generated by Happy from an 818-line `.y` grammar (136 named productions).
Port approach: translate each production to a function in `purescript-parsing` (`ParserT`-style).
The existing `ParserM` CPS monad in `CST/Monad.hs` can be adapted — it is already structured
as a `StateT`-like monad with error recovery. Use it directly and write grammar functions
that call each other recursively (recursive descent).

**Layout algorithm:**
`CST/Layout.hs` implements PureScript's indentation-sensitive layout rules, inserting virtual
`{`, `}`, `;` tokens. This is a pure token stream transformation — ports directly.

---

### Phase 3 — Desugaring / Name Resolution (Weeks 8–12)

| PureScript file | Haskell source | Notes |
|----------------|---------------|-------|
| `src/.../Sugar/Names/Common.purs` | `Sugar/Names/Common.hs` (68 LOC) | |
| `src/.../Sugar/Names/Env.purs` | `Sugar/Names/Env.hs` (502 LOC) | ImportEnvironment, Exports |
| `src/.../Sugar/Names/Imports.purs` | `Sugar/Names/Imports.hs` (229 LOC) | |
| `src/.../Sugar/Names/Exports.purs` | `Sugar/Names/Exports.hs` (306 LOC) | |
| `src/.../Sugar/Names.purs` | `Sugar/Names.hs` (443 LOC) | Name resolution pass |
| `src/.../Sugar/Operators/Common.purs` | `Sugar/Operators/Common.hs` (144 LOC) | |
| `src/.../Sugar/Operators/Expr.purs` | `Sugar/Operators/Expr.hs` (52 LOC) | |
| `src/.../Sugar/Operators/Types.purs` | `Sugar/Operators/Types.hs` (34 LOC) | |
| `src/.../Sugar/Operators/Binders.purs` | `Sugar/Operators/Binders.hs` (33 LOC) | |
| `src/.../Sugar/Operators.purs` | `Sugar/Operators.hs` (496 LOC) | Operator fixity/precedence |
| `src/.../Sugar/CaseDeclarations.purs` | `Sugar/CaseDeclarations.hs` (419 LOC) | Case desugaring |
| `src/.../Sugar/TypeClasses.purs` | `Sugar/TypeClasses.hs` (392 LOC) | Type class → dictionary |
| `src/.../Sugar/TypeClasses/Deriving.purs` | `Sugar/TypeClasses/Deriving.hs` (207 LOC) | |
| `src/.../Sugar/DoNotation.purs` | `Sugar/DoNotation.hs` (83 LOC) | do-notation desugaring |
| `src/.../Sugar/AdoNotation.purs` | `Sugar/AdoNotation.hs` (66 LOC) | ado-notation |
| `src/.../Sugar/LetPattern.purs` | `Sugar/LetPattern.hs` (54 LOC) | let patterns |
| `src/.../Sugar/ObjectWildcards.purs` | `Sugar/ObjectWildcards.hs` (101 LOC) | `{ foo: _ }` |
| `src/.../Sugar/TypeDeclarations.purs` | `Sugar/TypeDeclarations.hs` (97 LOC) | |
| `src/.../Sugar/BindingGroups.purs` | `Sugar/BindingGroups.hs` (305 LOC) | SCC + binding groups |
| `src/.../Sugar.purs` | `Sugar.hs` (75 LOC) | Orchestrator |

---

### Phase 4 — Type Checker (Weeks 12–20)

The largest and most complex phase. The type checker implements:
- Hindley-Milner type inference with bidirectional checking
- Row polymorphism (extensible records/variants)
- Type class entailment via dictionary passing
- Kind inference
- `Coercible` solving
- Deriving instances

Work in this order (dependency order):

| PureScript file | Haskell source | LOC | Notes |
|----------------|---------------|-----|-------|
| `src/.../Environment.purs` | `Environment.hs` | 687 | TypeClassData, type/kind env |
| `src/.../TypeChecker/Monad.purs` | `TypeChecker/Monad.hs` | 486 | CheckState, Check monad stack |
| `src/.../TypeChecker/Skolems.purs` | `TypeChecker/Skolems.hs` | 131 | Skolem variable generation |
| `src/.../TypeChecker/Synonyms.purs` | `TypeChecker/Synonyms.hs` | 63 | Type synonym expansion |
| `src/.../TypeChecker/Unify.purs` | `TypeChecker/Unify.hs` | 223 | Unification algorithm |
| `src/.../TypeChecker/Subsumption.purs` | `TypeChecker/Subsumption.hs` | 130 | Subtype/subsumption check |
| `src/.../TypeChecker/Roles.purs` | `TypeChecker/Roles.hs` | 263 | Role inference |
| `src/.../TypeChecker/Kinds.purs` | `TypeChecker/Kinds.hs` | 1021 | Kind inference |
| `src/.../TypeChecker/Entailment/IntCompare.purs` | `TypeChecker/Entailment/IntCompare.hs` | 102 | |
| `src/.../TypeChecker/Entailment/Coercible.purs` | `TypeChecker/Entailment/Coercible.hs` | 946 | Coercible solving |
| `src/.../TypeChecker/Entailment.purs` | `TypeChecker/Entailment.hs` | 923 | Type class solving |
| `src/.../TypeChecker/Deriving.purs` | `TypeChecker/Deriving.hs` | 837 | Instance deriving |
| `src/.../TypeChecker/Types.purs` | `TypeChecker/Types.hs` | 1040 | Core type inference |
| `src/.../TypeChecker.purs` | `TypeChecker.hs` | 797 | Top-level orchestration |

**Key monad stack** (`Check` in Haskell):
```
WriterT MultipleErrors (StateT CheckState (ExceptT MultipleErrors Identity))
```
Port as the same transformer stack using `purescript-transformers`.
`CheckState` contains fresh name supply (`Int`), the type environment, and solver state.

**Mutable state note:** The Haskell `Check` monad uses `StateT` (pure), not `IORef`.
This ports cleanly. Only the fresh-name supply needs care.

---

### Phase 5 — CoreFn + CodeGen (Weeks 20–24)

| PureScript file | Haskell source | LOC | Notes |
|----------------|---------------|-----|-------|
| `src/.../CoreFn/Ann.purs` | `CoreFn/Ann.hs` | 24 | Annotation type |
| `src/.../CoreFn/Meta.purs` | `CoreFn/Meta.hs` | 51 | IsConstructor, meta info |
| `src/.../CoreFn/Binders.purs` | `CoreFn/Binders.hs` | 42 | CoreFn binder ADT |
| `src/.../CoreFn/Expr.purs` | `CoreFn/Expr.hs` | 122 | CoreFn expression ADT |
| `src/.../CoreFn/Module.purs` | `CoreFn/Module.hs` | 25 | Module type |
| `src/.../CoreFn/Traversals.purs` | `CoreFn/Traversals.hs` | 86 | |
| `src/.../CoreFn/Desugar.purs` | `CoreFn/Desugar.hs` | 272 | Typed AST → CoreFn |
| `src/.../CoreFn/Laziness.purs` | `CoreFn/Laziness.hs` | 568 | Laziness analysis |
| `src/.../CoreFn/CSE.purs` | `CoreFn/CSE.hs` | 442 | Common subexpr elimination |
| `src/.../CoreFn/Optimizer.purs` | `CoreFn/Optimizer.hs` | 31 | |
| `src/.../CoreFn/ToJSON.purs` | `CoreFn/ToJSON.hs` | 249 | Use `codec-argonaut` |
| `src/.../CoreFn/FromJSON.purs` | `CoreFn/FromJSON.hs` | 319 | Use `codec-argonaut` |
| `src/.../CoreImp/AST.purs` | `CoreImp/AST.hs` | 242 | Imperative IR |
| `src/.../CoreImp/Module.purs` | `CoreImp/Module.hs` | 19 | |
| `src/.../CoreImp/Optimizer/Common.purs` | `CoreImp/Optimizer/Common.hs` | 72 | |
| `src/.../CoreImp/Optimizer/Unused.purs` | `CoreImp/Optimizer/Unused.hs` | 55 | |
| `src/.../CoreImp/Optimizer/Blocks.purs` | `CoreImp/Optimizer/Blocks.hs` | 28 | |
| `src/.../CoreImp/Optimizer/Inliner.purs` | `CoreImp/Optimizer/Inliner.hs` | 294 | |
| `src/.../CoreImp/Optimizer/TCO.purs` | `CoreImp/Optimizer/TCO.hs` | 191 | Tail call opt |
| `src/.../CoreImp/Optimizer/MagicDo.purs` | `CoreImp/Optimizer/MagicDo.hs` | 136 | `Effect` optimization |
| `src/.../CoreImp/Optimizer.purs` | `CoreImp/Optimizer.hs` | 85 | |
| `src/.../CodeGen/JS/Common.purs` | `CodeGen/JS/Common.hs` | 249 | JS identifier mangling |
| `src/.../CodeGen/JS/Printer.purs` | `CodeGen/JS/Printer.hs` | 310 | JS pretty-printer |
| `src/.../CodeGen/JS.purs` | `CodeGen/JS.hs` | 519 | CoreImp → JS AST |

---

### Phase 6 — Externs & In-Memory Make (Weeks 24–26)

**Externs format change:**
The Haskell compiler stores module interfaces as CBOR (`.cbor` files via `codec-serialise`).
Port switches to JSON using `codec-argonaut`. The JSON shape should be compatible with the
existing `CoreFn` JSON format where possible.

| PureScript file | Haskell source | LOC | Notes |
|----------------|---------------|-----|-------|
| `src/.../Externs.purs` | `Externs.hs` | 280 | ExternsFile, applyExternsFileToEnvironment |
| `src/.../Make/Cache.purs` | `Make/Cache.hs` | 149 | Content-hash rebuild decisions |
| `src/.../Make/Actions.purs` | `Make/Actions.hs` | 455 | MakeActions typeclass |
| `src/.../Make/Monad.purs` | `Make/Monad.hs` | 187 | Make monad (simplified, no IO) |
| `src/.../Make.purs` | `Make.hs` | 302 | Top-level `make` function |
| `src/.../ModuleDependencies.purs` | `ModuleDependencies.hs` | 89 | Dependency sort |
| `src/.../Renamer.purs` | `Renamer.hs` | 216 | Alpha-rename for codegen |
| `src/.../Graph.purs` | `Graph.hs` | 58 | SCC graph utilities |
| `src/.../Pretty/Types.purs` | `Pretty/Types.hs` | — | Type pretty-printing |
| `src/.../Pretty/Values.purs` | `Pretty/Values.hs` | — | Value pretty-printing |

**Virtual filesystem:**
Implement `MakeActions` backed by `Map String String` for browser use.
Expose through a thin FFI layer:
```purescript
-- In FFI:
foreign import readFileSync :: String -> Effect (Maybe String)
foreign import writeFileSync :: String -> String -> Effect Unit
```
For browser, these FFI functions read/write a JS `Map` object held in module scope,
pre-seeded with compiled Prelude modules at page load time.

---

### Phase 7 — Browser REPL (Weeks 26–28)

| PureScript file | Haskell source | Notes |
|----------------|---------------|-------|
| `src/.../Interactive/Types.purs` | `Interactive/Types.hs` | REPL state, command ADT |
| `src/.../Interactive/Parser.purs` | `Interactive/Parser.hs` | Parse REPL input |
| `src/.../Interactive/Module.purs` | `Interactive/Module.hs` | Module wrapping for REPL |
| `src/.../Interactive/Printer.purs` | `Interactive/Printer.hs` | Result printing |
| `src/.../Interactive.purs` | `Interactive.hs` (363 LOC) | `handleCommand` |
| `src/Browser/Main.purs` | (new) | Browser entry point |

**Browser entry point** (`Browser/Main.purs`):
```purescript
-- Exposed to JS as a callable API:
compileAndEval :: String -> Effect (Either String String)
```
- Accepts a snippet of PureScript source
- Wraps it in a `Main` module with a `main` binding
- Runs the full compile pipeline in-memory
- Returns the generated JavaScript string (or error message)
- The browser JS layer then `eval()`s the JS to run it

**Prelude bundling strategy:**
The browser needs compiled Prelude externs (JSON format) pre-loaded.
Build a script that:
1. Compiles all Prelude modules with the Haskell compiler
2. Outputs externs as JSON
3. Bundles them into the browser JS as a JSON literal

This gives the PS compiler-in-browser access to all Prelude type information
without recompiling the Prelude on every REPL interaction.

---

## Testing Strategy

### Unit testing (per phase)
After each phase, write `Test.*.purs` modules that exercise the new code.
Use `Test.Spec` (purescript-spec) as the test framework.

### Golden testing (integration)
The existing `tests/purs/passing/` directory contains hundreds of `.purs` files
with corresponding `.out` golden files. Write a test runner in PureScript that:
1. Reads each `.purs` file
2. Compiles it using the ported compiler
3. Compares output against `.out`

Start with the simplest cases (`tests/purs/passing/1000.purs` through `1020.purs` etc.)
and progressively expand coverage.

### Self-hosting bootstrap test
The ultimate verification:
1. Compile `purescript_port/src/**/*.purs` using the Haskell compiler → JS
2. Run the resulting JS compiler on `purescript_port/src/**/*.purs`
3. Compare the two outputs — they must be semantically equivalent

---

## Risks and Mitigations

| Risk | Likelihood | Mitigation |
|------|-----------|------------|
| Happy grammar → PS parser translation is buggy | High | Test against all 500+ golden passing tests continuously |
| TypeChecker subtle bugs (HM + rows + classes) | High | Run golden tests; diff against Haskell compiler output |
| Externs JSON format mismatch | Medium | Write round-trip tests from day 1 |
| Performance: PS compiler too slow in browser | Medium | Profile; use `Effect.Ref` for hot paths; lazy module loading |
| Bootstrapping: compiler can't compile itself | Medium | Keep self-referential cycles minimal; test incrementally |
| CBOR → JSON externs breaks ecosystem tools | Low | Keep Haskell compiler as primary; PS port is additive |

---

## Recommended PureScript Dependencies

```yaml
# spago.yaml (dependencies section)
dependencies:
  - prelude
  - effect
  - refs           # Effect.Ref (IORef equivalent)
  - ordered-collections   # Data.Map, Data.Set
  - either
  - maybe
  - tuples
  - lists
  - arrays
  - strings        # Data.String, Data.String.CodeUnits
  - unicode        # Data.CodePoint.Unicode
  - integers
  - numbers
  - nonempty
  - transformers   # StateT, ExceptT, WriterT, ReaderT
  - parsing        # Parser combinators (for CST/Parser.purs)
  - argonaut-core
  - argonaut-codecs
  - codec-argonaut # For externs JSON serialization
  - dodo-printer   # Pretty printing (replaces boxes)
  - spec           # Test framework
  - spec-discovery # Auto-discover tests
  - foldable-traversable
  - control
  - identity
  - newtype
  - profunctor
  - safe-coerce
```

---

## Directory Structure

```
purescript_port/
├── PORT_PLAN.md               ← this file
├── spago.yaml
├── spago.lock
├── src/
│   └── Language/
│       └── PureScript/
│           ├── Names.purs
│           ├── PSString.purs
│           ├── Label.purs
│           ├── Roles.purs
│           ├── Comments.purs
│           ├── Types.purs
│           ├── Errors.purs
│           ├── Environment.purs
│           ├── TypeClassDictionaries.purs
│           ├── Externs.purs
│           ├── ModuleDependencies.purs
│           ├── Renamer.purs
│           ├── Graph.purs
│           ├── Make.purs
│           ├── Interactive.purs
│           ├── Sugar.purs
│           ├── TypeChecker.purs
│           ├── Linter.purs
│           ├── AST/
│           │   ├── SourcePos.purs
│           │   ├── Literals.purs
│           │   ├── Binders.purs
│           │   ├── Declarations.purs
│           │   └── Traversals.purs
│           ├── CST/
│           │   ├── Types.purs
│           │   ├── Errors.purs
│           │   ├── Positions.purs
│           │   ├── Monad.purs
│           │   ├── Layout.purs
│           │   ├── Lexer.purs
│           │   ├── Parser.purs
│           │   ├── Utils.purs
│           │   ├── Flatten.purs
│           │   ├── Convert.purs
│           │   └── Print.purs
│           ├── Sugar/
│           │   ├── Names.purs
│           │   ├── Names/
│           │   │   ├── Common.purs
│           │   │   ├── Env.purs
│           │   │   ├── Imports.purs
│           │   │   └── Exports.purs
│           │   ├── Operators.purs
│           │   ├── Operators/
│           │   │   ├── Common.purs
│           │   │   ├── Expr.purs
│           │   │   ├── Types.purs
│           │   │   └── Binders.purs
│           │   ├── CaseDeclarations.purs
│           │   ├── TypeClasses.purs
│           │   ├── TypeClasses/
│           │   │   └── Deriving.purs
│           │   ├── DoNotation.purs
│           │   ├── AdoNotation.purs
│           │   ├── LetPattern.purs
│           │   ├── ObjectWildcards.purs
│           │   ├── TypeDeclarations.purs
│           │   └── BindingGroups.purs
│           ├── TypeChecker/
│           │   ├── Monad.purs
│           │   ├── Skolems.purs
│           │   ├── Synonyms.purs
│           │   ├── Unify.purs
│           │   ├── Subsumption.purs
│           │   ├── Roles.purs
│           │   ├── Kinds.purs
│           │   ├── Entailment.purs
│           │   ├── Entailment/
│           │   │   ├── Coercible.purs
│           │   │   └── IntCompare.purs
│           │   ├── Deriving.purs
│           │   ├── Types.purs
│           │   └── TypeSearch.purs
│           ├── CoreFn/
│           │   ├── Ann.purs
│           │   ├── Meta.purs
│           │   ├── Binders.purs
│           │   ├── Expr.purs
│           │   ├── Module.purs
│           │   ├── Traversals.purs
│           │   ├── Desugar.purs
│           │   ├── Laziness.purs
│           │   ├── CSE.purs
│           │   ├── Optimizer.purs
│           │   ├── ToJSON.purs
│           │   └── FromJSON.purs
│           ├── CoreImp/
│           │   ├── AST.purs
│           │   ├── Module.purs
│           │   └── Optimizer/
│           │       ├── Common.purs
│           │       ├── Unused.purs
│           │       ├── Blocks.purs
│           │       ├── Inliner.purs
│           │       ├── TCO.purs
│           │       ├── MagicDo.purs
│           │       └── Optimizer.purs
│           ├── CodeGen/
│           │   └── JS/
│           │       ├── Common.purs
│           │       ├── Printer.purs
│           │       └── JS.purs
│           ├── Make/
│           │   ├── Actions.purs
│           │   ├── Cache.purs
│           │   ├── Monad.purs
│           │   └── BuildPlan.purs   ← simplified, single-threaded
│           ├── Pretty/
│           │   ├── Types.purs
│           │   └── Values.purs
│           └── Interactive/
│               ├── Types.purs
│               ├── Parser.purs
│               ├── Module.purs
│               └── Printer.purs
├── src/
│   └── Browser/
│       └── Main.purs              ← browser entry point
└── test/
    ├── Main.purs
    └── Golden/
        └── Runner.purs
```

---

## Rough Timeline

| Phase | Weeks | LOC to port | Key risk |
|-------|-------|------------|---------|
| 0 — Scaffolding | 1–2 | 0 | Tooling choices |
| 1 — Core types | 2–4 | ~8,000 | Errors pretty-printing |
| 2 — CST frontend | 4–8 | ~4,000 | Happy → recursive descent |
| 3 — Sugar | 8–12 | ~4,000 | Name resolution subtleties |
| 4 — TypeChecker | 12–20 | ~6,300 | HM correctness, row poly |
| 5 — CoreFn + CodeGen | 20–24 | ~4,400 | Laziness/CSE algorithms |
| 6 — Make + Externs | 24–26 | ~1,500 | CBOR → JSON migration |
| 7 — Browser REPL | 26–28 | ~700 | Browser eval loop, prelude bundle |
| **Total** | **28 weeks** | **~28,900** | |

A small team (2–3 people) could complete this in roughly 6–9 months working full-time,
with the first end-to-end demo (simple expressions compiling in the browser) achievable
around week 24 if phases are parallelized.
