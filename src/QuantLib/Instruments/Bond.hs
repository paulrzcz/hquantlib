module QuantLib.Instruments.Bond
        ( module QuantLib.Instruments.Bond
        ) where

import QuantLib.Event
import QuantLib.Instruments.Instrument
import QuantLib.Time.Date

-- | Bond data type
data Bond = Bond {
        bbSettlementDays         :: Int,
        bbIssueDate              :: Date,
        bbCoupons                :: Leg
        } deriving (Show)

-- to be implemented
-- instance Instrument Bond where
        

