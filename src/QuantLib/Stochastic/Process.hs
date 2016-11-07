{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module QuantLib.Stochastic.Process
        ( module QuantLib.Stochastic.Process )
        where

import qualified Data.DList                 as D
import           QuantLib.Stochastic.Random (NormalGenerator (..))

-- | Discretization of stochastic process over given interval
class Discretize b where
        dDrift :: StochasticProcess a => a->b->Dot->Double
        dDiff  :: StochasticProcess a => a->b->Dot->Double
        dDt    :: StochasticProcess a => a->b->Dot->Double

-- | 1D Stochastic process
class StochasticProcess a where
        drift  :: a -> Dot -> Double
        diff   :: a -> Dot -> Double
        evolve :: Discretize b=> b -> a -> Dot -> Double -> Dot
        evolve discr p dot dw = Dot newT newX
                where   !newT = getT dot + dDt p discr dot
                        !newX = getX dot + dDrift p discr dot + dDiff p discr dot * dw

-- | Dot. t and x pair
data Dot = Dot { getT :: {-# UNPACK #-} !Double, getX :: {-# UNPACK #-} !Double }
        deriving (Show, Eq)

-- | Path as list of Dots
type Path = [Dot]

-- | Generates sample path for given stochastic process under discretization and normal generator for given amount of steps, starting from x0
generatePath :: (StochasticProcess a, NormalGenerator b, Discretize c) => b->c->a->Int->Dot->(Path, b)
generatePath rnd discr sp steps x0 =
  let (path, rng', _) = foldl generationF (D.singleton x0, rnd, x0) [1..steps]
  in (D.toList path, rng')
  where
    generationF :: NormalGenerator b => (D.DList Dot, b, Dot) -> Int -> (D.DList Dot, b, Dot)
    generationF (p, r, lastDot) _ =
      let (dw, r') = ngGetNext r
          newP = evolve discr sp lastDot dw
      in (D.snoc p newP, r', newP)

-- | Geometric Brownian motion
data GeometricBrownian = GeometricBrownian {
        gbDrift :: Double,
        gbDiff  :: Double
        } deriving (Show)

instance StochasticProcess GeometricBrownian where
        drift p (Dot _ x) = gbDrift p * x
        diff  p (Dot _ x) = gbDiff p  * x

-- | Ito process
data ItoProcess = ItoProcess {
        ipDrift :: Dot->Double,
        ipDiff  :: Dot->Double
        }

instance StochasticProcess ItoProcess where
        drift   = ipDrift
        diff    = ipDiff

-- | Square-root process
data SquareRootProcess = SquareRootProcess {
        srpSpeed :: Double,
        srpMean  :: Double,
        srpSigma :: Double
        } deriving (Show)

instance StochasticProcess SquareRootProcess where
       drift p (Dot _ x) = srpSpeed p * (srpMean p - x)
       diff  p (Dot _ x) = srpSigma p * sqrt x

-- | Ornstein-Uhlenbeck process
data OrnsteinUhlenbeckProcess = OrnsteinUhlenbeckProcess {
        oupSpeed :: Double,
        oupLevel :: Double,
        oupSigma :: Double
        } deriving (Show)

instance StochasticProcess OrnsteinUhlenbeckProcess where
        drift p (Dot _ x) = oupSpeed p * (oupLevel p - x)
        diff  p _         = oupSigma p

-- | Generalized Black-Scholes process
data BlackScholesProcess = BlackScholesProcess {
        bspRiskFree :: Double->Double,
        bspDividend :: Double->Double,
        bspBlackVol :: Dot->Double
        }

instance StochasticProcess BlackScholesProcess where
        drift (BlackScholesProcess r q v) dot = r (getT dot) - q ( getT dot) - 0.5 * v dot ** 2
        diff = bspBlackVol
