module QuantLib.PricingEngines
        ( module QuantLib.PricingEngines
        ) where

class PricingEngine a where
        peCalculate :: a->a
