module QuantLib.Stochastic.Random
        ( BoxMuller
        , createNormalGen
        , NormalGenerator (..)
        , module GSL.Random.Gen
        ) where

import GSL.Random.Gen
import Control.Monad

data BoxMuller = BoxMuller {
        bmFirst         :: Bool,
        bmSecondValue   :: Double,
        bmRng           :: RNG
        }

createNormalGen :: RNG->BoxMuller
createNormalGen r = BoxMuller {
        bmFirst         = True,
        bmSecondValue   = 0.0,
        bmRng           = r
        }

getRndList :: NormalGenerator a => a->Int->IO ([Double], a)
getRndList rnd n = do
        let ns = replicate n (1 :: Int)
        foldM foldFunc ([], rnd) ns
        where   foldFunc (xs, r) _ = do
                    (x, newRnd) <- ngGetNext r
                    return (xs++[x], newRnd)

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
                                let r = x1*x1 + x2*x2
                                if (r>=1.0 || r<=0.0) then
                                        getRs
                                else
                                        return (r, x1, x2)
                        
        ngGetNext (BoxMuller False s r) = do
                return (s, BoxMuller True s r)
