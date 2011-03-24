module QuantLib.Money
        ( module QuantLib.Money
        ) where

import QuantLib.Currency
import QuantLib.Currencies.Europe (eur)

-- | Amount of cash. Please, note that currency conversion is not implemented yet.
data Money = Money {
        mValue          :: Double,
        mCurrency       :: Currency
        } deriving (Eq)

instance Show Money where
        showsPrec _ (Money v c) s = show v++" "++show c++s

instance Num Money where
        (+) (Money v0 c0) (Money v1 c1)
                | c0 == c1      = Money (v0+v1) c0
                | otherwise     = error "Currency conversion is not implemented"
        (*) _ _         = error "Multiplying moneys has no sense"
        (-) (Money v0 c0) (Money v1 c1)
                | c0 == c1      = Money (v0-v1) c0
                | otherwise     = error "Currency conversion is not implemented"
        negate (Money v c)      = Money (-v) c
        abs    (Money v c)      = Money (abs v) c
        signum (Money v c)      = Money (signum v) c
        fromInteger     i       = Money (fromInteger i) eur
