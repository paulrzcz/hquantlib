
module QuantLib.Stochastic.Discretize
        ( module QuantLib.Stochastic.Discretize )
        where

import QuantLib.Stochastic.Process

-- | Euler discretization of stochastic processes
data Euler = Euler { eDt :: Double }
        deriving (Show, Eq)

-- | Euler end-point discretization of stochastic processes
data EndEuler = EndEuler { eeDt :: Double }
        deriving (Show, Eq)

instance Discretize Euler where
        dDrift p e dot = (drift p dot)*(eDt e)
        dDiff  p e dot = (diff  p dot)*sqrt (eDt e)
        dDt    _ e _   = eDt e

instance Discretize EndEuler where
        dDrift p e dot = (drift p nextDot)*(eeDt e)
                where nextDot = Dot ((getT dot) + (eeDt e)) (getX dot)
        dDiff  p e dot = (diff  p nextDot)*sqrt (eeDt e)
                where nextDot = Dot ((getT dot) + (eeDt e)) (getX dot) 
        dDt    _ e _   = eeDt e

