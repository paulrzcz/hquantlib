module QuantLib.Quotes
        ( module QuantLib.Quotes
        ) where

import Data.Maybe
import QuantLib.Options
import QuantLib.PricingEngines.BlackFormula

-- | Base type class for market observables
class Quote a where
        qValue :: a->Maybe Double
        pureValue :: a->Double
        pureValue x = fromMaybe 0.0 (qValue x)

-- | Market element returning a stored value
data SimpleQuote = SimpleQuote (Maybe Double)
        deriving (Show, Eq)

instance Quote SimpleQuote where
        qValue (SimpleQuote x) = x

-- | Market element whose value depends on two other market elements
data CompositeQuote a = CompositeQuote {
        -- | First element
        cqQuote1        :: a,
        -- | Second element
        cqQuote2        :: a,
        -- | Composition function
        cqComposite     :: a->a->Maybe Double
        }

instance Quote (CompositeQuote a) where
        qValue x        = cqComposite x (cqQuote1 x) (cqQuote2 x)

-- | Market element whose value depends on another quote
data DerivedQuote a = DerivedQuote {
        dqQuote         :: a,
        dqDerivateFunc  :: a->Maybe Double
        }

instance Quote (DerivedQuote a) where
        qValue x        = dqDerivateFunc x (dqQuote x)

-- | Quote for the implied standard deviation of an underlying
data ImpliedStdDevQuote a = ImpliedStdDevQuote {
        isdqOptionType  :: OptionType,
        isdqForward     :: a,
        isdqPrice       :: a,
        isdqStrike      :: Double,
        isdqGuess       :: Maybe Double
        } deriving (Show)

instance Quote a => Quote (ImpliedStdDevQuote a) where
        qValue (ImpliedStdDevQuote opType fwd price strike guess) 
                = blackFormulaImpliedStdDev opType (pureValue fwd) (pureValue price) strike 1.0 0.0 guess 1.0e-6 100

-- | Quote for the Eurodollar-future implied standard deviation
data EurodollarFutureQuote a = EurodollarFutureQuote {
        efqForward      :: a,
        efqCallPrice    :: a,
        efqPutPrice     :: a,
        efqStrike       :: Double,
        efqGuess        :: Maybe Double
        } deriving (Show)

instance Quote a => Quote (EurodollarFutureQuote a) where
        qValue (EurodollarFutureQuote forward callPrice putPrice strike guess)
                | strike > forwardValue = blackFormulaImpliedStdDev Call strike forwardValue putValue 1.0 0.0 guess 1.0e-6 100
                | otherwise     = blackFormulaImpliedStdDev Put strike forwardValue callValue 1.0 0.0 guess 1.0e-6 100
                where
                        forwardValue = 100.0 - fromMaybe 0.0 (qValue forward)
                        putValue     = fromMaybe 0.0 (qValue putPrice)
                        callValue    = fromMaybe 0.0 (qValue callPrice)


