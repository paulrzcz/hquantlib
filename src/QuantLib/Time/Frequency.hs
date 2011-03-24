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

module QuantLib.Time.Frequency
        ( module QuantLib.Time.Frequency
        ) where

import QuantLib.Time.TimeUnit

data Frequency
    = NoFrequency | Once | Annual | Semiannual | EveryFourthMonth | Quarterly
      | Bimonthly | Monthly | EveryFourthWeek | Biweekly | Weekly | Daily | OtherFrequency
      deriving (Eq,Show)

lookupFrequency :: Frequency -> Int
lookupFrequency freq = frequencyduration freq
    where frequencyduration fl
            = case fl of
               NoFrequency -> (-1)
               Once        -> 0
               Annual      -> 1
               Semiannual  -> 2
               EveryFourthMonth -> 3
               Quarterly   -> 4
               Bimonthly   -> 6
               Monthly     -> 12
               EveryFourthWeek -> 13
               Biweekly    -> 26
               Weekly      -> 52
               Daily       -> 365
               OtherFrequency -> 999

calcFrequency :: Int -> TimeUnit -> Frequency
calcFrequency 0 tunit | tunit == Years = Once
                      | otherwise      = NoFrequency
calcFrequency 1 Years  =  Annual
calcFrequency 6 Months = Semiannual
calcFrequency 4 Months = EveryFourthMonth
calcFrequency 3 Months = Quarterly
calcFrequency 2 Months = Bimonthly
calcFrequency 1 Months = Monthly
calcFrequency 4 Weeks = EveryFourthWeek
calcFrequency 2 Weeks = Biweekly
calcFrequency 1 Weeks = Weekly
calcFrequency 1 Days  = Daily
calcFrequency _ _     = OtherFrequency
