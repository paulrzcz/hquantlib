module QuantLib.Models.Volatility 
    ( Volatility
    , Estimation (..)
    , VolatilityEstimator (..)
    , VolatilityEstimatorAlgorithm (..)
    ) where

import QuantLib.Prices (IntervalPrice(..))
import QuantLib.TimeSeries (IntervalPriceSeries)

import qualified Data.Map as M
import Statistics.Sample (stdDev, fastVarianceUnbiased)
import qualified Data.Vector.Unboxed as U

-- | Volatility type
type Volatility = Double

-- | Estimation type with strictness as it is usually required only one 'Double' to process
data Estimation = Estimation {-# UNPACK #-} !Volatility
    deriving (Show, Eq)

-- | Type class of volatility estimators
class VolatilityEstimator algorithm where
    -- | The estimation procedure that takes a series of 'QuantLib.Prices.IntervalPrice'
    estimate :: algorithm -> IntervalPriceSeries -> Estimation

data VolatilityEstimatorAlgorithm = SimpleEstimator -- ^ Simple estimator with drift
    | SimpleDriftLessEstimator    -- ^ Simple estimator without drift
    | ParkinsonEstimator          -- ^ Parkinson number
    | GarmanKlass5Estimator       -- ^ Garman-Klass estimator
    | RogersSatchelEstimator      -- ^ Rogers-Stachel estimator
    | YangZhangEstimator          -- ^ Yang-Zhang estimator
    deriving (Show, Eq, Enum)

instance VolatilityEstimator VolatilityEstimatorAlgorithm where
    estimate ParkinsonEstimator       = parkinson
    estimate SimpleEstimator          = simple
    estimate SimpleDriftLessEstimator = simpleDriftLess
    estimate GarmanKlass5Estimator    = garmanKlass5
    estimate RogersSatchelEstimator   = rogersSatchel
    estimate YangZhangEstimator       = yangZhang

{- Private implementation -}

-- we assume that the array is already sorted by time stamp
toLogArray :: IntervalPriceSeries -> U.Vector Double
toLogArray prices = U.fromList $ zipWith delog bars (tail bars)
    where
        bars        = map snd $ M.toAscList prices
        delog x0 x1 = log (ipClose x1/ipClose x0)

simple :: IntervalPriceSeries -> Estimation
simple = Estimation . stdDev . toLogArray

simpleDriftLess :: IntervalPriceSeries -> Estimation
simpleDriftLess = Estimation . sqrt . divByN . U.foldl' accum (T 0.0 0) . toLogArray
    where
        accum (T a n) b = T (a + b*b) (n + 1)
        divByN (T a n)  = a / fromIntegral n

parkinson :: IntervalPriceSeries -> Estimation
parkinson = Estimation . sqrt . divByN . M.foldl' summate (T 0.0 0)
    where
        divByN (T a n) = a / (4*log 2) / fromIntegral n
        summate (T a n) (IntervalPrice _ l h _) = T (a + logBase l h ** 2) (n + 1)

garmanKlass5 :: IntervalPriceSeries -> Estimation
garmanKlass5 = Estimation . sqrt . combine . M.foldl' point (TT 0.0 0.0 0)
    where
        logConst = 2.0 * log 2.0 - 1.0
        combine (TT a b n) = (0.5*a - logConst*b) / fromIntegral n
        point (TT a b n) (IntervalPrice o l h c) = TT (a + logBase l h ** 2) (b + logBase o c ** 2) (n + 1)

rogersSatchel :: IntervalPriceSeries -> Estimation
rogersSatchel = Estimation . sqrt . varRS

varRS :: IntervalPriceSeries -> Double
varRS = combine . M.foldl' point (T 0.0 0)
    where
        combine (T a n) = a / fromIntegral n
        point (T a n) (IntervalPrice o h l c) = 
            T (a + logBase c h * logBase o h + logBase c l * logBase o l) (n + 1)

toSimpleLogWith :: (IntervalPrice -> Double) -> IntervalPriceSeries -> U.Vector Double
toSimpleLogWith f = U.fromList . map (f . snd) . M.toAscList

yangZhang :: IntervalPriceSeries -> Estimation
yangZhang prices = Estimation $ sqrt (varO + k * varC + (1.0 - k) * varRS prices)
    where
        n        = fromIntegral $ M.size prices
        k        = 0.34/(1.34 + (n + 1) / (n - 1))
        opens    = toSimpleLogWith (log . ipOpen) prices
        closes   = toSimpleLogWith (log . ipClose) prices
        varO     = fastVarianceUnbiased opens
        varC     = fastVarianceUnbiased closes

-- Strict data structure for efficient folds

data T = T {-# UNPACK #-} !Double {-# UNPACK #-} !Int
data TT = TT {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Int