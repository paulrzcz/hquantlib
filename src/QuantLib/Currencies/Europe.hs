module QuantLib.Currencies.Europe
        ( module QuantLib.Currencies.Europe
        ) where

import QuantLib.Currency

-- | Swiss france
chf :: Currency
chf = Currency {
        cName           = "Swiss franc",
        cCode           = "CHF",
        cIsoCode        = 756,
        cFracsPerUnit   = 100
        }

-- | Czech koruna
czk :: Currency
czk = Currency {
        cName           = "Czech koruna",
        cCode           = "CZK",
        cIsoCode        = 203,
        cFracsPerUnit   = 100
        }

-- | Danish krone
dkk :: Currency
dkk = Currency {
        cName           = "Danish krone",
        cCode           = "DKK",
        cIsoCode        = 208,
        cFracsPerUnit   = 100
        }

-- | European Euro
eur :: Currency
eur = Currency {
        cName           = "European Euro",
        cCode           = "EUR",
        cIsoCode        = 978,
        cFracsPerUnit   = 100
        }

-- | British pound sterling
gbp :: Currency
gbp = Currency {
        cName           = "British pound sterling",
        cCode           = "GBP",
        cIsoCode        = 826,
        cFracsPerUnit   = 100
        }

