module QuantLib.Instruments.Instrument
        (module QuantLib.Instruments.Instrument
        ) where

import QuantLib.Time.Date

class Instrument a where
        iNPV            :: a->Double
        iErrorEstimate  :: a->Double
        iDate           :: a->Date
        iIsExpired      :: a->Bool


