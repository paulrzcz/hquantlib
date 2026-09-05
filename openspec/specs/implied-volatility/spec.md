# implied-volatility Specification

## Purpose

Defines the behavioral contract for recovering the implied standard deviation of an underlying from a Black (option) price. Callers — implied-vol quotes and future-vol quotes — rely on validated inputs, numerically accurate converged results, and an honest `Nothing` when no solution can be determined.

## Requirements

### Requirement: Invalid inputs yield no implied volatility

The implied standard deviation function SHALL return `Nothing` when any input is out of its valid domain: negative Black price, non-positive discount, negative strike, non-positive forward, or negative displacement. For all other inputs it SHALL attempt a solution and return it wrapped in `Just`.

#### Scenario: Negative black price
- **WHEN** the function is called with `blackPrice = -1.0` and otherwise valid arguments
- **THEN** the result is `Nothing`

#### Scenario: Non-positive discount
- **WHEN** the function is called with `discount = 0.0` and otherwise valid arguments
- **THEN** the result is `Nothing`

#### Scenario: Non-positive forward
- **WHEN** the function is called with `forward = 0.0` and otherwise valid arguments
- **THEN** the result is `Nothing`

#### Scenario: Negative displacement
- **WHEN** the function is called with `displacement = -0.5` and otherwise valid arguments
- **THEN** the result is `Nothing`

### Requirement: Converged results are accurate to the requested tolerance

For any in-domain input with a reachable solution, the function SHALL return a standard deviation whose reproducing Black price matches the given price within the requested accuracy. The result SHALL NOT depend on the caller's initial guess beyond that tolerance, and the same inputs SHALL always produce the same output (pure function).

#### Scenario: Round trip from a known volatility
- **WHEN** a Black price is computed from a known standard deviation `x` with `forward = 100`, `strike = 105`, `discount = 0.95`, `displacement = 0`, and the function is called on that price with `accuracy = 1e-6`
- **THEN** the returned value is `Just x'` with `|x' - x| <= 1e-4`

#### Scenario: Determinism across guesses
- **WHEN** the function is called twice on the same inputs with different initial guesses
- **THEN** both calls return values equal within the requested accuracy

### Requirement: Both option types and displaced forwards are supported

The function SHALL handle both call and put option types, including positive displacement (Black-style formula on a displaced forward/strike), with the same accuracy guarantees.

#### Scenario: Put option round trip
- **WHEN** a Black put price is computed from a known standard deviation and the function is invoked with the put option type on that price
- **THEN** the known standard deviation is recovered within the requested accuracy

#### Scenario: Displaced diffusion round trip
- **WHEN** a price is computed from a known standard deviation with `displacement > 0` and the function is invoked on that price
- **THEN** the known standard deviation is recovered within the requested accuracy

### Requirement: Non-convergence yields no result instead of a wrong value

When no iteration converges to a solution of the requested accuracy within the iteration budget, the function SHALL return `Nothing`. It MUST NOT return an unconverged iterate or an arbitrary value.

#### Scenario: Unreachable price
- **WHEN** the function is called with a Black price that no standard deviation can reproduce (e.g. a call price exceeding the undiscounted forward) and a bounded iteration budget
- **THEN** the result is `Nothing`
