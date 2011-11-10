module QuantLib.Time.Calendars.Czech
        ( module QuantLib.Time.Calendars.Czech
        ) where

import QuantLib.Time.Date
import Data.Time.Calendar
import Data.Time.Calendar.Easter

data CzechCalendar = CzechCalendar

instance Holiday CzechCalendar where
        isHoliday _ (_, 1, 1)   = True
        isHoliday _ (_, 5, 1)   = True
        isHoliday _ (_, 5, 8)   = True
        isHoliday _ (_, 7, 5)   = True
        isHoliday _ (_, 7, 6)   = True
        isHoliday _ (_, 9,28)   = True
        isHoliday _ (_,10,28)   = True
        isHoliday _ (_,11,17)   = True
        isHoliday _ (_,12,24)   = True
        isHoliday _ (_,12,25)   = True
        isHoliday _ (_,12,26)   = True
        isHoliday _ (2004, 1, 2)= True
        isHoliday _ (2004,12,31)= True
        isHoliday _ (y, m, d)   = not (weekend || easter)
                where   weekend = isWeekEnd day
                        easter  = day == addDays 1 (gregorianEaster y)
                        day     = fromGregorian y m d
