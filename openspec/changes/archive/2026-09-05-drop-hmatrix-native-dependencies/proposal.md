# Proposal: Drop hmatrix native dependencies

## Why

The `hmatrix`, `hmatrix-gsl`, and `hmatrix-special` dependencies drag in system BLAS, LAPACK, and GSL, yet the entire library uses them for exactly two calls in one file (`src/QuantLib/PricingEngines/BlackFormula.hs`): `erf` for the normal CDF and `root DNewton` for the implied-vol Newton solve. Removing them makes the package 100% pure Haskell — buildable anywhere GHC builds, no native prerequisites on CI or user machines — and eliminates a GPL-3.0-only dependency (`hmatrix-gsl`) from this LGPL-3.0 project.

## What Changes

- Remove `hmatrix`, `hmatrix-gsl`, and `hmatrix-special` from `hquantlib.cabal` build-depends.
- Add `math-functions` (pure Haskell, BSD-2, already in the build closure as a transitive dependency of `statistics`).
- Replace `Numeric.GSL.Special.Erf.erf` with `Numeric.SpecFunctions.erf` from `math-functions`.
- Replace the GSL `root DNewton` call in `blackFormulaImpliedStdDev` with a pure Haskell solver (Newton with analytic vega; see design.md for the choice).
- On solver non-convergence, return `Nothing` instead of silently returning the last iterate (the current code discards GSL's `RootCode`). Signature `blackFormulaImpliedStdDev :: ... -> Maybe Double` is unchanged.
- Add golden and round-trip tests pinning the solver's numerical behavior (the existing test suite is a stub and would not catch a regression).

## Capabilities

### New Capabilities

- `implied-volatility`: Behavioral contract of `blackFormulaImpliedStdDev` — input validation to `Nothing`, convergence/accuracy guarantees, honest `Nothing` on non-convergence, and round-trip consistency with Black pricing.

### Modified Capabilities

(none — no existing specs)

## Impact

- **Code**: `src/QuantLib/PricingEngines/BlackFormula.hs` (only consumer of the hmatrix family), `hquantlib.cabal` (dependency list; `QuantLib.Quotes` API is unchanged).
- **Dependencies**: −`hmatrix`, −`hmatrix-gsl`, −`hmatrix-special`; +`math-functions` (pure Haskell, no native libs).
- **Build/CI**: `.drone.yml` GSL/BLAS/LAPACK `apt install` prerequisites become unnecessary for this package (may still be needed by other consumers; not changed here).
- **Licensing**: removes the GPL-3.0-only `hmatrix-gsl` edge from an LGPL-3.0 codebase.
- **Behavior**: solver failures now yield `Nothing` instead of an arbitrary iterate; converged results match the old solver within its configured accuracy (`1.0e-6` at call sites).
