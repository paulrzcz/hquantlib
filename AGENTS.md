# AGENTS.md

Haskell port of QuantLib. Single-package library: all code lives in `src/` under the `QuantLib.*` namespace.

## Commands

```sh
cabal build        # primary verification (this is all CI runs)
cabal test         # NOTE: test suite is a stub (one trivial property) — passing tests prove nothing
cabal run mctest   # Monte Carlo demo; prints histogram as CSV lines to stdout
```

- CI (`.drone.yml`) builds with cabal on GHC 9.4.8. `stack.yaml` (lts-18.28, GHC 8.10) is stale — don't use stack.
- Library builds with `-Wall` — fix warnings in code you touch.

## Native dependencies

`hmatrix` / `hmatrix-gsl` / `hmatrix-special` require system BLAS, LAPACK, and GSL. On Debian (as in CI): `apt install libblas-dev liblapack-dev libgsl-dev`. Build fails at dependency setup if these are missing.

## Gotchas

- No automatic module discovery: new files must be added to `exposed-modules` or `other-modules` in `hquantlib.cabal` or they won't build.
- Tests (`src/Test.hs`) and the demo (`src/Tests/McTest.hs`) live inside `src/` (`hs-source-dirs: src`), not `test/`/`app/`.
- Date/calendar logic (DayCounter etc.) lives in the external `hquantlib-time` package (>= 0.0.5.1), not in this repo.
- The `optimize` cabal flag (default: on) adds `-funbox-strict-fields -fspec-constr -fdicts-cheap`; disable with `cabal build -f -optimize` for faster iteration.
