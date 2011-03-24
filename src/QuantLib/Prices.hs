module QuantLib.Prices
        ( module QuantLib.Prices
        ) where

data PriceType = Bid | Ask | Last | Close | Mid | MidEq | MidSafe
        deriving (Show, Eq)

data IntervalPrice = IntervalPrice {
        ipOpen  :: Double,
        ipHigh  :: Double,
        ipLow   :: Double,
        ipClose :: Double
        } deriving (Show, Eq)


