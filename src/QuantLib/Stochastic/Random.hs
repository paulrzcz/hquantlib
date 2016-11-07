{-# LANGUAGE BangPatterns #-}
module QuantLib.Stochastic.Random
        ( BoxMuller
--        , createNormalGen
        , mkNormalGen
        , NormalGenerator (..)
        , InverseNormal
        , mkInverseNormal
        ) where

import           QuantLib.Math.InverseNormal
import           System.Random.Mersenne.Pure64

-- | Box-Muller method
data BoxMuller = BoxMuller {
        bmFirst       :: Bool,
        bmSecondValue :: Double,
        bmRng         :: PureMT
        }

mkNormalGen ::  IO BoxMuller
mkNormalGen = do
        rng <- newPureMT
        return $! createNormalGen rng

-- | Creates normally distributed generator
createNormalGen :: PureMT -> BoxMuller
createNormalGen r = BoxMuller {
        bmFirst         = True,
        bmSecondValue   = 0.0,
        bmRng           = r
        }

-- | Normally distributed generator
class NormalGenerator a where
        ngGetNext :: a -> (Double, a)
        ngMkNew   :: a -> IO a

instance NormalGenerator BoxMuller where
        ngMkNew _       = mkNormalGen
        ngGetNext = boxMullerGetNext

boxMullerGetNext :: BoxMuller -> (Double, BoxMuller)
boxMullerGetNext (BoxMuller True _ rng) = (s1*ratio, bm)
  where
        (!r, !s1, !s2, rng') = getRs
        !ratio = boxMullerRatio r
        !bm = BoxMuller {
                bmFirst         = False,
                bmSecondValue   = s2*ratio,
                bmRng           = rng'
                }
        getRs = if r'>=1.0 || r'<=0.0 then getRs else (r', s1', s2', rng2)
          where
              (x1, rng1) = randomDouble rng
              (x2, rng2) = randomDouble rng1
              s1' = 2.0*x1-1.0
              s2' = 2.0*x2-1.0
              r' = s1*s1 + s2*s2
boxMullerGetNext (BoxMuller False !s !r) = (s, BoxMuller True s r)

{-# ANN boxMullerRatio "NoHerbie" #-}
boxMullerRatio :: Double -> Double
boxMullerRatio r = sqrt (-2.0 * log r / r)

-- | Normal number generation using inverse cummulative normal distribution
newtype InverseNormal = InverseNormal PureMT

mkInverseNormal ::  IO InverseNormal
mkInverseNormal = do
        rng <- newPureMT
        return $! InverseNormal rng

instance NormalGenerator InverseNormal where
        ngMkNew _       = mkInverseNormal
        ngGetNext (InverseNormal rng)   =
                let (x, rng') = randomDouble rng
                in (inverseNormal x, InverseNormal rng')
