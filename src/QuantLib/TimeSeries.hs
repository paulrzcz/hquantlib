module QuantLib.TimeSeries
        ( module QuantLib.TimeSeries
        ) where

import QuantLib.Time.Date
import qualified Data.Map as M

-- | Time series
type TimeSeries m = M.Map Date m
