module QuantLib.Instruments.Stock
        ( module QuantLib.Instruments.Stock
        ) where

import QuantLib.Instruments.Instrument
import Data.Time.LocalTime

data Stock = Stock {
        sQuote  :: Double,
        sDate   :: LocalTime
        } deriving (Show)

instance Instrument Stock where
       iNPV             = sQuote
       iErrorEstimate _ = 0.0
       iDate            = sDate
       iIsExpired     _ = False

data Option = PutOption | CallOption
