
module QuantLib.Currency
        ( module QuantLib.Currency
        )
        where

-- | Currency specification
data Currency = Currency {
        -- | currency name, e.g. "U.S. dollar"
        cName           :: String,
        -- | ISO 4217 three-letter code, e.g. "USD"
        cCode           :: String,
        -- | ISO 4217 numeric code, e.g. 840
        cIsoCode        :: Integer,
        -- | number of fractionary parts in a unit
        cFracsPerUnit   :: Integer
        } deriving (Eq)

instance Show Currency where
        showsPrec _ x s = (cCode x)++s
