module QuantLib.Options
        ( module QuantLib.Options
        ) where

data OptionType = Call | Put
        deriving (Show, Eq)

toInt :: OptionType -> Int
toInt Call = 1
toInt Put  = -1

toDouble :: OptionType -> Double
toDouble Call = 1.0
toDouble Put  = -1.0
