module QuantLib.Event
        (module QuantLib.Event
        ) where

import QuantLib.Prices
import QuantLib.Time.Date

class Event a where
        evDate          :: a->Date
        evOccured       :: a->Date->Bool
        evOccured event date = evDate event < date

        evOccuredInclude:: a->Date->Bool
        evOccuredInclude event date = evDate event <= date

        evCompare :: a->a->Ordering
        evCompare x y
                | evDate x == evDate y = EQ
                | evDate x <= evDate y = LT
                | otherwise            = GT

        evEqual :: a->a->Bool
        evEqual x y = evDate x == evDate y

-- | Cash flows data type
data CashFlow = CashFlow {
        cfDate          :: Date,
        cfAmount        :: Double
        } deriving (Show)

instance Event CashFlow where
        evDate (CashFlow date _) = date

instance Eq CashFlow where
       (==) = evEqual

instance Ord CashFlow where
        compare = evCompare

-- | Sequence of cash-flows
type Leg        = [CashFlow]

data Callability = Call {
        cPrice  :: CallPrice,
        cDate   :: Date
        } | Put {
        cPrice  :: CallPrice,
        cDate   :: Date
        } deriving (Show)

instance Event Callability where
        evDate = cDate

instance Eq Callability where
        (==) = evEqual

instance Ord Callability where
        compare = evCompare
