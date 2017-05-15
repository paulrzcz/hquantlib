{-# LANGUAGE BangPatterns #-}
module QuantLib.Stochastic.Random
        ( BoxMuller
        , createNormalGen
        , mkNormalGen
        , NormalGenerator (..)
        , InverseNormal
        , mkInverseNormal
        ) where

import           QuantLib.Math.InverseNormal

import           QuantLib.Stochastic.PureMT

class RandomGenerator a where
  create :: IO a
  next   :: a -> (Double, a)
  split  :: a -> (a, a)
  split  = splitWithSeed 1
  splitWithSeed :: Integer -> a -> (a, a)

instance RandomGenerator PureMT where
  create = newPureMT
  next   = randomDouble
  splitWithSeed = splitMTwithSeed

-- | Box-Muller method
data BoxMuller a = BoxMuller {
        bmFirst       :: Bool,
        bmSecondValue :: Double,
        bmRng         :: a
        }

mkNormalGen :: RandomGenerator a => IO (BoxMuller a)
mkNormalGen = do
        rng <- create
        return $! createNormalGen rng

-- | Creates normally distributed generator
createNormalGen :: RandomGenerator a => a -> BoxMuller a
createNormalGen r = BoxMuller {
        bmFirst         = True,
        bmSecondValue   = 0.0,
        bmRng           = r
        }

-- | Normally distributed generator
class NormalGenerator a where
        ngGetNext :: a -> (Double, a)
        ngMkNew   :: a -> IO a
        ngSplit   :: a -> (a, a)
        ngSplit   = ngSplitWithSeed 1
        ngSplitWithSeed :: Integer -> a -> (a, a)

instance RandomGenerator a => NormalGenerator (BoxMuller a) where
        ngMkNew _       = mkNormalGen
        ngGetNext = boxMullerGetNext
        ngSplitWithSeed seed  x   = (x { bmRng = rng1 }, x { bmRng = rng2 })
          where
              (rng1, rng2) = splitWithSeed seed (bmRng x)

boxMullerGetNext :: RandomGenerator a => BoxMuller a -> (Double, BoxMuller a)
boxMullerGetNext (BoxMuller True _ rng) = (s1*ratio, BoxMuller {
                                            bmFirst         = False,
                                            bmSecondValue   = s2*ratio,
                                            bmRng           = g2
                                          })
        where
          (x1, g1) = next rng
          (x2, g2) = next g1
          (r, s1, s2) = getRs
          ratio = boxMullerRatio r
          getRs =
            let
              as1 = 2.0*x1-1.0
              as2 = 2.0*x2-1.0
              ar = s1*s1 + s2*s2
            in
              if r>=1.0 || r<=0.0 then getRs else (ar, as1, as2)

boxMullerGetNext (BoxMuller False !s !r) = (s, BoxMuller True s r)

{-# ANN boxMullerRatio "NoHerbie" #-}
boxMullerRatio :: Double -> Double
boxMullerRatio r = sqrt (-2.0 * log r / r)

-- | Normal number generation using inverse cummulative normal distribution
newtype InverseNormal a = InverseNormal a

mkInverseNormal ::  RandomGenerator a => IO (InverseNormal a)
mkInverseNormal = do
        rng <- create
        return $! InverseNormal rng

instance RandomGenerator a => NormalGenerator (InverseNormal a) where
        ngMkNew _       = mkInverseNormal
        ngGetNext (InverseNormal rng)   = (inverseNormal x, InverseNormal newRng)
          where (x, newRng) = next rng
        ngSplitWithSeed seed (InverseNormal x) = (InverseNormal x1, InverseNormal x2)
          where
            (x1, x2) = splitWithSeed seed x
