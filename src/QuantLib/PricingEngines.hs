{-# LANGUAGE MultiParamTypeClasses #-}
module QuantLib.PricingEngines
        ( module QuantLib.PricingEngines
        ) where

import QuantLib.Event

class Event e => PricingEngine a e where
        peCalculate :: e->a->e
