module QuantLib.Time.Date
        ( module QuantLib.Time.Date
        ) where

import Data.Time
import Data.Time.Calendar.WeekDate

{- | Business Day conventions
 - These conventions specify the algorithm used to adjust a date in case it is not a valid business day.
 -}
data BusinessDayConvention = Following 
        | ModifiedFollowing 
        | Preceding
        | ModifiedPreceding
        | Unadjusted
        deriving (Show, Eq, Enum)

-- | Week days
data WeekDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
        deriving (Show, Eq, Enum)

-- | Date
type Date = Day

-- | Defines a holidays for given calendar. Corresponds to calendar class in QuantLib
class Holiday m where
        isHoliday :: m->(Integer, Int, Int)->Bool
        
        isBusinessDay :: m->Date->Bool
        isBusinessDay m d = not (isHoliday m $ toGregorian d)

        hBusinessDayBetween :: m->(Date, Date)->Int
        hBusinessDayBetween m (fd, td) = foldl countDays 0 listOfDates
                where   countDays counter x     = counter + fromEnum (isBusinessDay m x)
                        listOfDates             = getDaysBetween (fd, td)

-- | Gets a week day 
getWeekDay :: Date->WeekDay
getWeekDay d   = toEnum (weekDay - 1)
        where   (_, _, weekDay) = toWeekDate d

-- | Generate a list of all dates inbetween
getDaysBetween ::  (Day, Day) -> [Day]
getDaysBetween (fd, td) = reverse $ generator fd []
        where   generator date x
                        | date < td     = generator nextDate (nextDate : x)
                        | otherwise     = x
                        where   nextDate        = addDays 1 date

-- | Checks if the day is a weekend, i.e. Saturday or Sunday
isWeekEnd :: Date->Bool
isWeekEnd d     = (weekday == Saturday) || (weekday == Sunday)
        where   weekday = getWeekDay d

-- | Gets the next working day
getNextBusinessDay :: Holiday a => a->Date->Date
getNextBusinessDay m d
        | isBusinessDay m nextDay       = nextDay
        | otherwise                     = getNextBusinessDay m nextDay
        where   nextDay = addDays 1 d
