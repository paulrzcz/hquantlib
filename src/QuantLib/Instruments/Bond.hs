{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module QuantLib.Instruments.Bond
        ( module QuantLib.Instruments.Bond
        ) where

import QuantLib.Event
import QuantLib.Instruments.Instrument
import QuantLib.Time.Date
import QuantLib.PricingEngines
import QuantLib.Cashflows.CashflowFunctions (cfIsExpired)

-- | Bond data type
class Bond b where
        bbSettlementDays         :: b->Int
        bbIssueDate              :: b->Date
        bbCoupons                :: b->Leg
        bbMaturityDate           :: b->Date
        bbMaturityDate b = (cfDate . last . bbCoupons) b 
        -- | Theoretical bond yield
        bbYield                  :: b->Double
        -- | Theoretical clean price
        bbCleanPrice             :: b->Double
        -- | Theoretical dirty price
        bbDirtyPrice             :: b->Double

data SimpleBond = SimpleBond {
        sbSettlementDays :: Int,
        sbIssueDate      :: Date,
        sbCoupons        :: Leg
        } deriving (Show)

instance Bond SimpleBond where
       bbSettlementDays b = sbSettlementDays b
       bbIssueDate      b = sbIssueDate b
       bbCoupons        b = sbCoupons b

       bbYield         _ = 0.0
       bbCleanPrice    _ = 0.0
       bbDirtyPrice    _ = 0.0

instance Instrument SimpleBond where
        iNPV (SimpleBond sd id c)               = undefined
        iErrorEstimate (SimpleBond sd id c)     = undefined
        iDate (SimpleBond sd id c)              = undefined
        iIsExpired (SimpleBond sd id c)         = undefined
