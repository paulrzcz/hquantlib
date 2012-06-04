{-# LANGUAGE ExistentialQuantification #-}
module QuantLib.Priceable
    ( Priceable (..)
    ) where

-- | All instruments and events have a net present value
class Priceable a where
    npv             :: a -> Double
    errorEstimate   :: a -> Double

