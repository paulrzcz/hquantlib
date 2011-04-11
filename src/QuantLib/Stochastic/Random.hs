module QuantLib.Stochastic.Random
        ( BoxMuller
        , createNormalGen
        , NormalGenerator (..)
        , module GSL.Random.Gen
        ) where

import GSL.Random.Gen
import Control.Monad

-- | Box-Muller method
data BoxMuller = BoxMuller {
        bmFirst         :: Bool,
        bmSecondValue   :: Double,
        bmRng           :: RNG
        }

-- | Creates normally distributed generator
createNormalGen :: RNG->BoxMuller
createNormalGen r = BoxMuller {
        bmFirst         = True,
        bmSecondValue   = 0.0,
        bmRng           = r
        }

-- | Generates a list of normally distributed number using generator
getRndList :: NormalGenerator a => a->Int->IO ([Double], a)
getRndList rnd n = do
        let ns = replicate n (1 :: Int)
        foldM foldFunc ([], rnd) ns
        where   foldFunc (xs, r) _ = do
                    (x, newRnd) <- ngGetNext r
                    return (xs++[x], newRnd)

-- | Normally distributed generator
class NormalGenerator a where
        ngGetNext :: a -> IO (Double, a)

instance NormalGenerator BoxMuller where
        ngGetNext (BoxMuller True _ rng) = do
                (r, s1, s2) <- getRs
                let ratio = sqrt (-2.0*(log r)/r)
                let bm = BoxMuller {
                        bmFirst         = False,
                        bmSecondValue   = s2*ratio,
                        bmRng           = rng
                        }
                return (s1*ratio, bm)
                where   getRs = do
                                x1 <- getUniformPos rng
                                x2 <- getUniformPos rng
                                let s1 = 2.0*x1-1.0
                                let s2 = 2.0*x2-1.0
                                let r = s1*s1 + s2*s2
                                if (r>=1.0 || r<=0.0) then
                                        getRs
                                else
                                        return (r, s1, s2)
                        
        ngGetNext (BoxMuller False s r) = do
                return (s, BoxMuller True s r)
