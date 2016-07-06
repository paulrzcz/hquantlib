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
import           System.Random.Mersenne

-- | Box-Muller method
data BoxMuller = BoxMuller {
        bmFirst       :: Bool,
        bmSecondValue :: Double,
        bmRng         :: MTGen
        }

mkNormalGen ::  IO BoxMuller
mkNormalGen = do
        rng <- newMTGen Nothing
        return $! createNormalGen rng

-- | Creates normally distributed generator
createNormalGen :: MTGen->BoxMuller
createNormalGen r = BoxMuller {
        bmFirst         = True,
        bmSecondValue   = 0.0,
        bmRng           = r
        }

-- | Normally distributed generator
class NormalGenerator a where
        ngGetNext :: a -> IO (Double, a)
        ngMkNew   :: a -> IO a

instance NormalGenerator BoxMuller where
        ngMkNew _       = mkNormalGen
        ngGetNext = boxMullerGetNext

boxMullerGetNext :: BoxMuller -> IO (Double, BoxMuller)
boxMullerGetNext (BoxMuller True _ rng) = do
        (!r, !s1, !s2) <- getRs
        let !ratio = boxMullerRatio r
        let !bm = BoxMuller {
                bmFirst         = False,
                bmSecondValue   = s2*ratio,
                bmRng           = rng
                }
        return (s1*ratio, bm)
        where   getRs = do
                        x1 <- random rng :: IO Double
                        x2 <- random rng :: IO Double
                        let !s1 = 2.0*x1-1.0
                        let !s2 = 2.0*x2-1.0
                        let !r = s1*s1 + s2*s2
                        if r>=1.0 || r<=0.0 then getRs else return (r, s1, s2)
boxMullerGetNext (BoxMuller False !s !r) = return (s, BoxMuller True s r)

{-# ANN boxMullerRatio "NoHerbie" #-}
boxMullerRatio :: Double -> Double
boxMullerRatio r = sqrt (-2.0 * log r / r)

-- | Normal number generation using inverse cummulative normal distribution
data InverseNormal = InverseNormal MTGen

mkInverseNormal ::  IO InverseNormal
mkInverseNormal = do
        rng <- newMTGen Nothing
        return $! InverseNormal rng

instance NormalGenerator InverseNormal where
        ngMkNew _       = mkInverseNormal
        ngGetNext gen@(InverseNormal rng)   = do
                x <- random rng :: IO Double
                return (inverseNormal x, gen)
