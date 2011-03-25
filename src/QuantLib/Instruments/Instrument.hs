module QuantLib.Instruments.Instrument
        (module QuantLib.Instruments.Instrument
        ) where

import Data.Time.LocalTime

class Instrument a where
        iNPV            :: a->Double
        iErrorEstimate  :: a->Double
        iDate           :: a->LocalTime
        iIsExpired      :: a->Bool


