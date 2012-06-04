module QuantLib.Instruments.Stock
        ( module QuantLib.Instruments.Stock
        ) where

import Data.Time.LocalTime
import QuantLib.Instruments.Instrument
import QuantLib.Priceable

-- | Single stock instrument 
data Stock = Stock {
        sQuote  :: Double,
        sDate   :: LocalTime
        } deriving (Show)

instance Instrument Stock where
       iDate            = sDate
       iIsExpired     _ = False

instance Priceable Stock where
    npv (Stock q _)     = q
    errorEstimate _     = 0.0
