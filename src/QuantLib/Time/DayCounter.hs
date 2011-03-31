module QuantLib.Time.DayCounter
        ( module QuantLib.Time.DayCounter
        ) where

import QuantLib.Time.Date
import Data.Time.Calendar

class DayCounter m where
        dcName          :: m->String
        dcCount         :: m->Date->Date->Int
        dcYearFraction  :: m->Date->Date->Double

data SimpleDayCounter = SimpleDayCounter

instance DayCounter SimpleDayCounter where
        dcName _        = "Simple"
        dcCount         = undefined
        dcYearFraction  = undefined

data Thirty360 = ThirtyUSA | ThirtyEuropean | ThirtyItalian

instance DayCounter Thirty360 where
        dcName ThirtyUSA        = "Thirty USA"
        dcName ThirtyEuropean   = "Thirty Euro"
        dcName ThirtyItalian    = "Thirty Italian"

        dcYearFraction  dc fromDate toDate = (fromIntegral $ dcCount dc fromDate toDate)/360.0

        dcCount ThirtyUSA fd td = 360*(yy2-yy1) + 30*(mm2-mm1-1) + (max 0 (30-dd1)) + (min 30 dd2)
                where   (y1, mm1, dd1) = toGregorian fd
                        (y2, m2, d2)   = toGregorian td
                        yy1            = fromIntegral y1
                        yy2            = fromIntegral y2
                        (dd2, mm2)     = adjust dd1 d2 m2
                        adjust x1 x2 y2
                                | x2 == 31 && x1 < 30   = (1, y2+1)
                                | otherwise             = (x2, y2)


        dcCount ThirtyEuropean fd td = 360*(yy2-yy1) + 30*(m2-m1-1) + (max 0 (30-d1)) + (min 30 d2)
                where   (y1, m1, d1)    = toGregorian fd
                        (y2, m2, d2)    = toGregorian td
                        yy1             = fromIntegral y1
                        yy2             = fromIntegral y2

        dcCount ThirtyItalian fd td = 360*(yy2-yy1) + 30*(mm2-mm1-1) + (max 0 (30-dd1)) + (min 30 dd2)
                where   (y1, mm1, d1)   = toGregorian fd
                        (y2, mm2, d2)   = toGregorian td
                        yy1             = fromIntegral y1
                        yy2             = fromIntegral y2
                        dd1             = adjust d1 mm1
                        dd2             = adjust d2 mm2
                        adjust x1 y1
                                | y1 == 2 && x1 > 27    = 30
                                | otherwise             = x1

