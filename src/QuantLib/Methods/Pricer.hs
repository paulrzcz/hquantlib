{-# LANGUAGE BangPatterns #-}
module QuantLib.Methods.Pricer
      ( MaxMinClosePricer (..)
      , LastPointPricer (..)
      , LogLastPointPricer (..)
      ) where

import           QuantLib.Methods.MonteCarlo (PathPricer (..))
import           QuantLib.Stochastic.Process (Dot (..))

data MaxMinClosePricer = MMCP {
        mmcpHigh  :: Double,
        mmcpLow   :: Double,
        mmcpClose :: Double
        } deriving (Show)

instance PathPricer MaxMinClosePricer where
        ppPrice _ path = MMCP high low close
                where   !close   = last xs
                        !high    = maximum xs
                        !low     = minimum xs
                        xs      = map getX path

-- | This pricer gets the last point of path
newtype LastPointPricer = LastPointPricer Double

instance PathPricer LastPointPricer where
        ppPrice _ = LastPointPricer <$> getX . last

-- | This pricer estimates the log of difference between start and end of process
newtype LogLastPointPricer = LogLastPointPricer Double

instance PathPricer LogLastPointPricer where
        ppPrice _ path = LogLastPointPricer (log (lastX / firstX))
          where
            lastX = getX $ last path
            firstX = getX $ head path
