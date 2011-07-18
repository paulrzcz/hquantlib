module QuantLib.Methods.McExperimental
        ( module QuantLib.Methods.McExperimental
        ) where

import Control.Monad
import qualified Control.Monad.MC as MC
import QuantLib.Stochastic.Process

-- Components:
-- 1. Path generator by normal distribution

generateNormalPath :: (MC.MonadMC m) => Int -> m [Double]
generateNormalPath n =  mapM (\_ -> MC.normal 0.0 1.0) [1..n]

-- 2. Path valuation
-- 3. Summary



