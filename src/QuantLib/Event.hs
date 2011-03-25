module QuantLib.Event
        (module QuantLib.Event
        ) where

import Data.Time.LocalTime
import QuantLib.Prices

class Event a where
        evDate          :: a->LocalTime
        evOccured       :: a->LocalTime->Bool
        evOccured event date = (evDate event) < date

        evOccuredInclude:: a->LocalTime->Bool
        evOccuredInclude event date = (evDate event) <= date

        evCompare :: a->a->Ordering
        evCompare x y     
                | (evDate x) == (evDate y)      = EQ
                | (evDate x) <= (evDate y)      = LT
                | otherwise                     = GT

        evEqual :: a->a->Bool
        evEqual x y = (evDate x) == (evDate y)

data CashFlow = CashFlow {
        cfDate          :: LocalTime,
        cfAmount        :: Double
        } deriving (Show)

instance Event CashFlow where
        evDate (CashFlow date _) = date

instance Eq CashFlow where
       (==) = evEqual

instance Ord CashFlow where
        compare = evCompare

data Callability = Call { 
        cPrice  :: CallPrice,
        cDate   :: LocalTime
        } | Put {
        cPrice  :: CallPrice,
        cDate   :: LocalTime
        } deriving (Show)

instance Event Callability where
        evDate = cDate

instance Eq Callability where
        (==) = evEqual

instance Ord Callability where
        compare = evCompare
