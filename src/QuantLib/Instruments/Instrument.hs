{-# LANGUAGE ExistentialQuantification #-}
module QuantLib.Instruments.Instrument
    ( Instrument (..)
    , CompositeInstrument (..)
    ) where

import Data.Time.LocalTime
import qualified Data.Map as M
import QuantLib.Priceable

-- | Instrument type class
class Instrument a where
        iDate           :: a -> LocalTime
        iIsExpired      :: a -> Bool

-- | Composite instrument is an aggregate of other instruments.
data CompositeInstrument = forall a . (Instrument a, Priceable a) => CompositeInstrument (M.Map a Double) 

instance Priceable CompositeInstrument where
        npv (CompositeInstrument xs)   = M.foldrWithKey (\k x y -> y + npv k * x) 0.0 xs
        errorEstimate _                = 0.0

instance Instrument CompositeInstrument where
        iDate (CompositeInstrument xs)  = (iDate . head . M.keys) xs
        iIsExpired (CompositeInstrument xs) = (any iIsExpired . M.keys) xs
