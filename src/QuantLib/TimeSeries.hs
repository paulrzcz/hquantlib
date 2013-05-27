module QuantLib.TimeSeries
        ( module QuantLib.TimeSeries
        ) where

import QuantLib.Prices(IntervalPrice)
import Data.Time.LocalTime
import qualified Data.Map as M

-- | Time series
type TimeSeries m = M.Map LocalTime m

-- | Interval price time series
type IntervalPriceSeries = TimeSeries IntervalPrice