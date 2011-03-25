module QuantLib.Instruments.Stock
        ( module QuantLib.Instruments.Stock
        ) where

import QuantLib.Instruments.Instrument
import QuantLib.Time.Date

data Stock = Stock {
        sQuote  :: Double,
        sDate   :: Date
        } deriving (Show)

instance Instrument Stock where
       iNPV             = sQuote
       iErrorEstimate _ = 0.0
       iDate            = sDate
       iIsExpired     _ = False

data Option = PutOption | CallOption
