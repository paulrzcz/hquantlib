# Design: Drop hmatrix native dependencies

## Context

All hmatrix-family usage is confined to `src/QuantLib/PricingEngines/BlackFormula.hs`:

- `Numeric.GSL.Special.Erf.erf` — one call inside `cdf`, the standard normal CDF.
- `Numeric.GSL.Root.root DNewton` — one call solving a 1D objective (`blackImpliedStdDevHelper`) for implied std dev; the returned `RootCode` is currently discarded.

`hmatrix` itself is declared in `hquantlib.cabal` but imported nowhere. `hmatrix-gsl` is GPL-3.0-only while hquantlib is LGPL-3.0-only. `statistics` (already a direct dependency) depends on `math-functions` — a pure-Haskell, BSD-2 package exposing `Numeric.SpecFunctions.erf` and 1D root finding — so adding it costs nothing new in the build closure. See proposal.md for motivation; the behavior contract to satisfy is specs/implied-volatility.

## Goals / Non-Goals

**Goals:**
- Zero native (C) library prerequisites for building hquantlib.
- Same public API: `blackFormulaImpliedStdDev` keeps its exact type and module.
- Numerical equivalence within call-site accuracy (`1.0e-6`) for all currently-converging inputs.
- Honest `Nothing` on non-convergence (spec: "Non-convergence yields no result").

**Non-Goals:**
- Not touching `.drone.yml` (CI's GSL/BLAS installs may serve other repos/jobs; pruning is a separate change if desired).
- Not reworking `QuantLib.Quotes` (the suspicious argument order at `Quotes.hs:55` is a separate investigation).
- Not replacing other pure dependencies (`statistics`, `mersenne-random-pure64`, etc.).
- No solver performance tuning beyond what correctness requires.

## Decisions

### D1: Replace `erf` with `Numeric.SpecFunctions.erf` (math-functions)

Pure Haskell (default flag path), full double precision, already in the build closure, BSD-2. The local `cdf x = 0.5 * (1 + erf (x / sqrt 2))` stays as-is with only the import swapped.

*Alternative considered:* `Statistics.Distribution.Normal.cumulative` — avoids `erf` entirely but changes the CDF implementation for the whole module; the `erf` swap is more surgical.

### D2: Hand-rolled Newton iteration with analytic vega

GSL's `DNewton` differentiates the objective numerically; but the objective's derivative is the Black vega in closed form: `dPrice/dStdDev = (forward + displacement) * phi(d1) * discount`-adjusted (matching the objective's sign convention in `blackImpliedStdDevHelper`). Newton with the existing Cornish-Fisher-style initial guess typically converges in 2–5 iterations. ~15 lines in the same module; `blackImpliedStdDevHelper` is refactored minimally to expose `(price, vega)` per iterate instead of the GSL `[Double] -> [Double]` residual-list shape.

*Alternatives considered:*
- *Bisection* — bulletproof (monotone objective) but needs a bracket and converges linearly; kept as a documented fallback inside Newton if a step leaves the positive domain (clamp to a small positive floor), so no separate solver is needed.
- *`math-functions` `Numeric.RootFinding` (Ridders/bisection)* — zero new code, but requires a bracketing range that implied vol does not naturally provide; less QuantLib-like.

### D3: Convergence check derived from the same `accuracy`/`maxIter` parameters

Iteration stops when `|residual| <= accuracy` or `maxIter` is exhausted; exhaustion returns `Nothing`. The existing guards (negative price etc.) are unchanged and keep precedence. This slightly tightens behavior (previously failure returned the last iterate) — that is the spec'd improvement, not a regression.

### D4: Verification via golden + round-trip tests, not the stub suite

- **Round-trip property**: generate `(opType, forward, strike, displacement, discount, x)` grids/randoms, price at std dev `x` with the module's own formula, solve, assert `|x' − x| <= 1e-4`.
- **Golden values**: a small table of inputs → implied std dev, recorded from the current GSL implementation *before* the swap (this is the only step that must happen while the old code still builds; CI's GSL is available for that run if the local machine lacks it).
- `cabal build` with `-Wall` clean; `cabal run mctest` as a smoke test.

## Risks / Trade-offs

- [Golden values could embed GSL quirks at extreme inputs] → Tolerances are set at `1e-6` (call-site accuracy); inputs in the golden table stay in realistic ranges; round-trip property covers the wide grid.
- [Newton may overshoot for deep-ITM/tiny-vega inputs where GSL's DNewton also struggled] → Clamp candidate to a positive floor each step; fall back to bisection-style bracketing between iterates if progress stalls; on exhaustion return `Nothing` (spec allows only `Nothing`).
- [`math-functions` default `system-erf` flag uses libc `erf` on some platforms] → libc `erf` is correctly rounded per POSIX intent and math-functions' own fallback is full-precision; either path meets the spec tolerance. No action.
- [Downstream reverse deps might rely on the sloppy last-iterate behavior] → Implausible (it returns wrong numbers silently); documented in proposal as an intentional behavior change.

## Migration Plan

1. Record golden values from the current GSL implementation (needs GSL only for this step).
2. Apply dependency + solver swap in one commit.
3. `cabal build && cabal test && cabal run mctest` on a machine *without* GSL/BLAS to prove purity.
4. Rollback: revert the single commit; no data or API migration involved.

## Open Questions

None.
