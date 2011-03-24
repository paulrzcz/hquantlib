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


module QuantLib.Time.DayCounter
        ( module QuantLib.Time.Date
        , module QuantLib.Time.DayCounter
        ) where

import QuantLib.Time.Date

-- QuantLib defines a Time type which is typedef'd from double (a.k.a. Real)
type Time = Double

-- a DayCounter object holds two fields of function type - a function for calculating the daycount, and
-- a function for calculating the year fraction.  For each type of day counter, we create a DayCounter
-- object with the fields set to the functions corresponding to that type of day counter.
data DayCounter
    = DayCounter
        {
            dcDayCount :: Date -> Date -> Int,
            dcYearFraction :: Date -> Date -> Date -> Date -> Time,
            dcName :: String
        }

-- constructor function for day counter objects
makeDayCounter :: (Date -> Date -> Int) -> (Date -> Date-> Date -> Date -> Time) -> String -> DayCounter
makeDayCounter dycnt yrfrac name = DayCounter dycnt yrfrac name

-- generic day counter functions, which, when called with a day counter object, apply the
-- appropriate function within the object to the dates supplied
dayCount :: DayCounter -> Date -> Date -> Int
dayCount dc d1 d2 = (dcDayCount dc) d1 d2

yearFraction :: DayCounter -> Date -> Date -> Date -> Date -> Time
yearFraction dc d1 d2 d3 d4 = (dcYearFraction dc) d1 d2 d3 d4

dayCounterName :: DayCounter -> String
dayCounterName dc = dcName dc

-- some day counters use a default day counter function
defaultDayCount :: Date -> Date -> Int
defaultDayCount d1 d2 = subtractDates d2 d1        

-- actual year fraction calculation
-- yearTotal is the number of days to use as the denominator in the year fraction (e.g., 360 or 365)
actualyrfraction :: (Date -> Date -> Int) -> Double -> Date -> Date -> Date -> Date -> Time
actualyrfraction dayCount yearTotal d1 d2 d3 d4
    = (fromIntegral (dayCount d1 d2)) / yearTotal

-- Day counter objects
-- These day counters correspond to the QuantLib day counters with the same names
    
-- Actual 365 (Fixed) counter --

actual365fixedDayCounter :: DayCounter
actual365fixedDayCounter
    = makeDayCounter defaultDayCount (actualyrfraction defaultDayCount 365) "Actual/365 (Fixed)"

-- Actual 360 counter --

actual360DayCounter :: DayCounter
actual360DayCounter
    = makeDayCounter defaultDayCount (actualyrfraction defaultDayCount 360) "Actual/360"


-- Thirty/360 counter

data Convention = USA | BondBasis | European | EuroBondBasis | Italian

-- day count functions for the different conventions for thirty/360                    
                    
thirty360dayCountImplUS :: Date -> Date -> Int
thirty360dayCountImplUS (Date d1 m1 y1 s1) (Date d2 m2 y2 s2)
    = 360* (y2 - y1) + 30 * (m2adjusted - m1 - 1) + (max 0 (30-d1)) + (min 30 d2adjusted)
      where mustadjust = d2 == 31 && d1 < 30
            (d2adjusted,m2adjusted) = if mustadjust then (1,m2+1) else (d2,m2)

thirty360dayCountImplEU :: Date -> Date -> Int
thirty360dayCountImplEU (Date d1 m1 y1 s1) (Date d2 m2 y2 s2)
    = 360* (y2 - y1) + 30 * (m2 - m1 - 1) + (max 0 (30-d1)) + (min 30 d2)

thirty360dayCountImplItal :: Date -> Date -> Int
thirty360dayCountImplItal (Date d1 m1 y1 s1) (Date d2 m2 y2 s2)
    = 360* (y2 - y1) + 30 * (m2 - m1 - 1) + (max 0 (30-day1)) + (min 30 day2)
      where day1 = if (m1 == 2 && d1 > 27) then 30 else d1
            day2 = if (m2 == 2 && d2 > 27) then 30 else d2
                 
thirty360DayCounter :: Convention -> DayCounter
thirty360DayCounter conv = DayCounter daycount yrfraction name
    where (daycount,name) = case conv of
                            USA -> (thirty360dayCountImplUS,"30/360 (Bond Basis)")
                            BondBasis -> (thirty360dayCountImplUS,"30/360 (Bond Basis)")
                            European -> (thirty360dayCountImplEU,"30E/360 (Eurobond Basis)")
                            EuroBondBasis -> (thirty360dayCountImplEU,"30E/360 (Eurobond Basis)")
                            Italian -> (thirty360dayCountImplItal,"30/360 (Italian)")
          yrfraction      = actualyrfraction daycount 360

thirty360DayCounterDefault = thirty360DayCounter USA
          
-- test data

testdate1 = makeDate 02 07 2010
testdate2 = makeDate 10 07 2010
testdate3 = makeDate 19 07 2010
testdate4 = makeDate 31 07 2010
       
