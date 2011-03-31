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
        -- | Theoretical bond yield
        bbYield                  :: b->Double
        -- | Theoretical clean price
        bbCleanPrice             :: b->Double
        -- | Theoretical dirty price
        bbDirtyPrice             :: b->Double


