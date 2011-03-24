module QuantLib.TimeSeries
        ( module QuantLib.TimeSeries
        ) where

import QuantLib.Time.Date
import Data.Map

type TimeSeries m = Map Date m
