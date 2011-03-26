module QuantLib.Instruments.Instrument
        (module QuantLib.Instruments.Instrument
        ) where

import Data.Time.LocalTime
import qualified Data.Map as M

-- | Instrument type class
class Instrument a where
        iNPV            :: a->Double
        iErrorEstimate  :: a->Double
        iDate           :: a->LocalTime
        iIsExpired      :: a->Bool

-- | Composite instrument is an aggregate of other instruments.
data Instrument a => CompositeInstrument a = CompositeInstrument (M.Map a Double) 
        deriving (Show)

instance Instrument a => Instrument (CompositeInstrument a) where
        iNPV (CompositeInstrument xs)   = M.foldrWithKey (\k x y -> y + (iNPV k)*x) 0.0 xs
        iErrorEstimate _                = 0.0
        iDate (CompositeInstrument xs)  = (iDate . head . M.keys) xs
        iIsExpired (CompositeInstrument xs) = (any iIsExpired . M.keys) xs
