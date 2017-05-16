# HQuantLib #

HQuantLib is intended to be a port of [QuantLib](http://quantlib.org) in Haskell. It is not one-to-one port of the library but rather it is a re-implementation of ideas leveraging current libraries available in Haskell Platform.

The latest version implements:

1. Currencies (major only)
2. Time: Thirty360 DayCounter
3. Base 1D stochastic processes: Geometric Brownian, generic Ito process, square-root, Ornstein-Uhlenbeck, generalized Black-Scholes
4. Instruments: Bonds and Stocks
5. Monte Carlo engine for 1D processes
6. Volatility estimators: simple local estimator, Garman-Klass simple sigma and Parkinson sigma.
7. Copulas : Clayton, Max, Min, Ali-Mikhail-Haq and Farlie-Gumbel-Morgenstern

# Version 0.0.4.0 #
Monte Carlo engine has been moved to new Haskell-native RNG.
