module QuantLib.Prices
        ( module QuantLib.Prices
        ) where

-- | Price types
data PriceType = Bid | Ask | Last | Close | Mid | MidEq | MidSafe
        deriving (Show, Eq)

-- | Interval price
data IntervalPrice = IntervalPrice {
        ipOpen  :: Double,
        ipHigh  :: Double,
        ipLow   :: Double,
        ipClose :: Double
        } deriving (Show, Eq)


