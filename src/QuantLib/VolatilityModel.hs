module QuantLib.VolatilityModel
        ( module QuantLib.VolatilityModel
        ) where

import QuantLib.Prices
import QuantLib.TimeSeries
import qualified Data.Map as M

-- | Volatility type
type Volatility = Double

-- | Volatility time series
type VolatilitySeries = TimeSeries Volatility

-- | The estimator of time series of doubles
class DoubleVolatilityEstimator a where
        dveCalculate :: a->TimeSeries Double->VolatilitySeries

-- | The calculator of volatility for interval price
class IntervalPointCalculator a where
        ipcCalculatePoint :: a->IntervalPrice->Volatility

-- | Interval price volatility estimator
class IntervalVolatilityEstimator a where
        iveCalculate :: IntervalPointCalculator b => a->b->TimeSeries IntervalPrice->VolatilitySeries

-- | Simple local estimator
data SimpleLocalEstimator = SimpleLocalEstimator {
        sleYearFraction :: Double
        } deriving (Show, Eq)

instance DoubleVolatilityEstimator SimpleLocalEstimator where
        dveCalculate (SimpleLocalEstimator yf) series = M.fromList result
                where
                       (result, _) =  M.foldrWithKey volFunc ([], Nothing) series
                       volFunc _ s (xs, Nothing) = (xs, Just s)
                       volFunc k s (xs, Just s0) = ((k, estimator s0 s):xs, Just s)
                       estimator s0 s1 = abs (log (s1 / s0) ) / yf

-- | Garman-Klass interval estimators
data GarmanKlass = GarmanKlass {
        gkYearFraction  :: Double
        } deriving (Show, Eq)

instance IntervalVolatilityEstimator GarmanKlass where
        iveCalculate (GarmanKlass yf) ipc series = M.fromList result
                where   result = M.foldrWithKey volFunc [] series
                        volFunc k s xs = (k, abs (calculatePoint s) / yf):xs
                        calculatePoint = ipcCalculatePoint ipc

-- | Types of Garman-Klass estimators
data GarmanKlassPoint = GarmanKlassSimpleSigma
        | ParkinsonSigma

instance IntervalPointCalculator GarmanKlassPoint where
        ipcCalculatePoint GarmanKlassSimpleSigma (IntervalPrice open _ _ close) =  log (close/open) ** 2

        ipcCalculatePoint ParkinsonSigma (IntervalPrice o h l _) = (u-d)**2 / 4.0 / log 2.0
                where   u = log (h/o)
                        d = log (l/o)
