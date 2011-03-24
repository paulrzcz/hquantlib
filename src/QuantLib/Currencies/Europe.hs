module QuantLib.Currencies.Europe
        ( module QuantLib.Currencies.Europe
        ) where

import QuantLib.Currency

chf :: Currency
chf = Currency {
        cName           = "Swiss franc",
        cCode           = "CHF",
        cIsoCode        = 756,
        cFracsPerUnit   = 100
        }

czk :: Currency
czk = Currency {
        cName           = "Czech koruna",
        cCode           = "CZK",
        cIsoCode        = 203,
        cFracsPerUnit   = 100
        }

dkk :: Currency
dkk = Currency {
        cName           = "Danish krone",
        cCode           = "DKK",
        cIsoCode        = 208,
        cFracsPerUnit   = 100
        }

eur :: Currency
eur = Currency {
        cName           = "European Euro",
        cCode           = "EUR",
        cIsoCode        = 978,
        cFracsPerUnit   = 100
        }

gbp :: Currency
gbp = Currency {
        cName           = "British pound sterling",
        cCode           = "GBP",
        cIsoCode        = 826,
        cFracsPerUnit   = 100
        }

