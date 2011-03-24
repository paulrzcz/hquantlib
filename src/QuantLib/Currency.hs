
module QuantLib.Currency
        ( module QuantLib.Currency
        )
        where

data Currency = Currency {
        cName           :: String,
        cCode           :: String,
        cIsoCode        :: Integer,
        cFracsPerUnit   :: Integer
        } deriving (Eq)

instance Show Currency where
        showsPrec _ x s = (cCode x)++s
