module QuantLib.Stochastic.PureMT
  (
    PureMT
  , newPureMT
  , randomDouble
  , splitMT
  , splitMTwithSeed
  ) where

import           Data.Time.Calendar
import           Data.Time.Clock
import           System.CPUTime
import qualified System.Random.Mersenne.Pure64 as P

data PureMT = PureMT P.PureMT Integer

newPureMT :: IO PureMT
newPureMT = do
    ct <- getCPUTime
    t  <- getCurrentTime
    let seed = toModifiedJulianDay (utctDay t) + diffTimeToPicoseconds (utctDayTime t) + ct
    return $ PureMT (P.pureMT $ fromIntegral seed) seed

randomDouble:: PureMT -> (Double, PureMT)
randomDouble (PureMT mt seed) = (r, PureMT newMt seed)
  where
    (r, newMt) = P.randomDouble mt

splitMT :: PureMT -> (PureMT, PureMT)
splitMT = splitMTwithSeed 1

splitMTwithSeed :: Integer -> PureMT -> (PureMT, PureMT)
splitMTwithSeed addedSeed mt@(PureMT _ seed) = (mt, PureMT newMt newSeed)
  where
    newSeed = seed + addedSeed
    newMt = P.pureMT $ fromIntegral newSeed
