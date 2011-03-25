module QuantLib.TimeSeries
        ( module QuantLib.TimeSeries
        ) where

import Data.Time.LocalTime
import qualified Data.Map as M

-- | Time series
type TimeSeries m = M.Map LocalTime m
