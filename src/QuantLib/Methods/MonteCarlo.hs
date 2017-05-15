{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module QuantLib.Methods.MonteCarlo
        ( module QuantLib.Methods.MonteCarlo
        ) where

import           Control.Monad               ()
import           Control.Parallel.Strategies
import           QuantLib.Stochastic.Process
import           QuantLib.Stochastic.Random

-- | Summary type class aggregates all priced values of paths
class PathPricer p => Summary m p | m->p where
        -- | Updates summary with given priced pathes
        sSummarize      :: m->[p]->m
        -- | Defines a metric, i.e. calculate distance between 2 summaries
        sNorm           :: m->m->Double

-- | Path generator is a stochastic path generator
class PathGenerator m where
        pgMkNew         :: m->IO m
        pgGenerate      :: Integer -> m->Path

-- | Path pricer provides a price for given path
class PathPricer m where
        ppPrice         :: m->Path->m


-- | Monte Carlo engine function
monteCarlo :: (Summary s p, PathGenerator g) => PathMonteCarlo s p g->Int->s
monteCarlo (PathMonteCarlo s p g) size = sSummarize s priced
  where
        !priced = map pricing [1..size]
        pricing seed = ppPrice p (pgGenerate (fromIntegral seed) g)

-- | Monte Carlo engine function. Parallelized version
monteCarloParallel :: (Summary s p, PathGenerator g) => PathMonteCarlo s p g->Int->s
monteCarloParallel (PathMonteCarlo s p g) size = sSummarize s priced
  where
        !priced = map pricing [1..size] `using` rpar
        pricing seed = ppPrice p (pgGenerate (fromIntegral seed) g)

-- | Path-dependant Monte Carlo engine
data PathMonteCarlo s p g =
        PathMonteCarlo {
                pmcSummary   :: s,
                pmcPricer    :: p,
                pmcGenerator :: g
        }

-- | This pricer gets the last point of path
newtype LastPointPricer = LastPointPricer Dot

instance PathPricer LastPointPricer where
        ppPrice _ path = LastPointPricer (last path)

-- | Stochastic process generator
data ProcessGenerator sp b d =
        ProcessGenerator {
                pgStart      :: Dot,
                pgLength     :: Int,
                pgProcess    :: sp,
                pgGenerator  :: b,
                pgDiscretize :: d
        }

instance (StochasticProcess sp, NormalGenerator b, Discretize d) => PathGenerator (ProcessGenerator sp b d) where
        pgMkNew (ProcessGenerator start len process rnd d)       = do
                newRnd <- ngMkNew rnd
                return $! ProcessGenerator start len process newRnd d
        pgGenerate seed (ProcessGenerator start len sp b d) = generatePath newB d sp len start
          where (_, newB) = ngSplitWithSeed seed b
