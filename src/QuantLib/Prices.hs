module QuantLib.Prices
        ( PriceType (..)
        , CallPrice (..)
        , IntervalPrice (..)
        ) where

-- | Price types
data PriceType = Bid | Ask | Last | Close | Mid | MidEq | MidSafe
        deriving (Show, Eq)

-- | Call price
data CallPrice = DirtyPrice {
        cpPrice         :: Double
        } | CleanPrice {
        cpPrice         :: Double
        } deriving (Show, Eq, Ord)

-- | Interval price
data IntervalPrice = IntervalPrice {
        ipOpen  :: Double,
        ipHigh  :: Double,
        ipLow   :: Double,
        ipClose :: Double
        } deriving (Show, Eq)