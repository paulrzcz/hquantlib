{-
    Copyright (C) 2010, Simon Courtenage (courtenage@gmail.com)
    
    This file is part of QuantHas, an open-source Haskell implementation
    of the QuantLib library for quantitative finance.
    
    Quanthas is free software: you can redistribute it and/or modify it
    under the terms of the QuantHas license.  You should have received a
    copy of the license along with this program.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the license for more details.
    
-}

module QuantLib.Time.Period
        ( module QuantLib.Time.Frequency
        , module QuantLib.Time.Period) where

import QuantLib.Time.Frequency
import QuantLib.Time.TimeUnit

-- unlike quantlib, we store the frequency in the period object to save recalculating it later
data Period = Period { 
        periodLength :: Int, 
        periodUnits :: TimeUnit, 
        periodFrequency :: Frequency 
        }  deriving (Eq, Show)

-- use this as a wrapper round the different values that can be used to create a period value
data PeriodArgs = PeriodTimeArgs Int TimeUnit 
        | PeriodFreqArg Frequency

-- unwrap the arguments and use the data constructor to switch to the appropriate constructor
makePeriod :: PeriodArgs -> Period
makePeriod (PeriodTimeArgs len tunit) = makePeriodFromTime len tunit
makePeriod (PeriodFreqArg freq) = makePeriodFromFrequency freq

makePeriodFromTime :: Int -> TimeUnit -> Period
makePeriodFromTime n tunit = Period n tunit (calcFrequency (abs n) tunit)

makePeriodFromFrequency :: Frequency -> Period
makePeriodFromFrequency p@NoFrequency = Period 0 Days p
makePeriodFromFrequency p@Once        = Period 0 Years p
makePeriodFromFrequency p@Annual      = Period 1 Years p
makePeriodFromFrequency p@Semiannual  = Period (12 `div` (lookupFrequency p)) Months p
makePeriodFromFrequency p@EveryFourthMonth = Period (12 `div` (lookupFrequency p)) Months p
makePeriodFromFrequency p@Quarterly   = Period (12 `div` (lookupFrequency p)) Months p
makePeriodFromFrequency p@Bimonthly   = Period (12 `div` (lookupFrequency p)) Months p
makePeriodFromFrequency p@Monthly     = Period (12 `div` (lookupFrequency p)) Months p
makePeriodFromFrequency p@EveryFourthWeek = Period (52 `div` (lookupFrequency p)) Weeks p
makePeriodFromFrequency p@Biweekly    = Period (52 `div` (lookupFrequency p)) Weeks p
makePeriodFromFrequency p@Weekly      = Period (52 `div` (lookupFrequency p)) Weeks p
makePeriodFromFrequency p@Daily       = Period 1 Days p
makePeriodFromFrequency _             = error "makePeriodFromFrequency - OtherFrequency specified"

-- normalize function

normalize :: Period -> Period
normalize p@(Period 0 _ _ )       = p
normalize p@(Period len Days _) 
        | len `mod` 7 == 0 = Period newlen Weeks (calcFrequency newlen Weeks)
        | otherwise        = p
        where newlen = len `div` 7
normalize p@(Period len Months _) 
        | len `mod` 12 == 0 = Period newlen Years (calcFrequency newlen Years)
        | otherwise           = p
                                                           where newlen = len `div` 12
normalize p@(Period _ _ _) = p                                                           
