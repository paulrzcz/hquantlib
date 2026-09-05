# Tasks: Drop hmatrix native dependencies

## 1. Golden baseline (must run while GSL-backed code still builds)

- [ ] 1.1 On a machine/CI with GSL available, run a small script (ghci or a temporary executable) over a grid of realistic inputs (opType × moneyness, incl. one displaced case) and record `blackFormulaImpliedStdDev` outputs to a checked-in golden table module; verify the table compiles and covers both Call and Put plus displacement > 0

## 2. Dependency swap

- [ ] 2.1 In `hquantlib.cabal`, remove `hmatrix`, `hmatrix-gsl`, `hmatrix-special` and add `math-functions` with realistic bounds; verify `cabal build` resolves the plan without any native library configure errors

## 3. Solver replacement in BlackFormula.hs

- [ ] 3.1 Swap `Numeric.GSL.Special.Erf` for `Numeric.SpecFunctions (erf)`; verify `cdf` compiles unchanged and golden CDF spot values match (e.g. `cdf 0 == 0.5`, `cdf 1.96 ≈ 0.975`)
- [ ] 3.2 Refactor `blackImpliedStdDevHelper` to expose price and analytic vega per iterate, replacing the GSL `[Double] -> [Double]` residual shape; verify `-Wall` clean build
- [ ] 3.3 Replace `root DNewton` with hand-rolled Newton (positive-floor clamp, bisection fallback on stall), returning `Nothing` when `maxIter` is exhausted without meeting `accuracy`; keep all existing input guards with unchanged precedence; verify signature is unchanged
- [ ] 3.4 Add round-trip QuickCheck property (price at known std dev, recover within 1e-4 across opType/forward/strike/displacement/discount) and golden-table HUnit comparisons; wire into the existing `main-test` suite and verify `cabal test` passes
- [ ] 3.5 Add a non-convergence HUnit case (call price exceeding undiscounted forward) asserting `Nothing`; verify `cabal test` passes

## 4. End-to-end verification

- [ ] 4.1 `cabal build` with `-Wall` (flag `optimize` on) and confirm zero warnings in touched code; verify with `cabal build -f -optimize` as well
- [ ] 4.2 `cabal run mctest` and confirm histogram CSV output is produced as before
- [ ] 4.3 On an environment without GSL/BLAS/LAPACK installed, confirm `cabal build` succeeds end-to-end (proves purity; this local machine already reproduces the failure mode)
