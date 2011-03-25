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

module QuantLib.Time.Calendar
        ( module QuantLib.Time.Calendar
        ) where

import Data.Array
import QuantLib.Time.Date
import QuantLib.Time.BusinessDayConvention()

data Calendar = Calendar
                    {
                        calendarName :: String,
                        calendarIsBusinessDay :: Calendar -> Date -> Bool,
                        calendarIsWeekend :: Date -> Bool,
                        calendarIsHoliday :: Date -> Bool,
                        calendarImpl :: CalendarImpl
                    }

data CalendarImpl = CalendarImpl
                    {
                        -- | isEaster takes the number of the day in the year, the year
                        --   and returns whether or not that day is Easter Monday
                        calImplIsEasterMonday :: Int -> Year -> Bool,
                        -- | returns day of the year Easter Monday falls on
                        calImplGetEasterMonday :: Year -> Int,
                        calImplIsWeekend :: DayName -> Bool
                    }


westernCalendarImpl ::  CalendarImpl
westernCalendarImpl = CalendarImpl isWesternEaster getWesternEasterMonday isOrthodoxWeekend


orthodoxCalendarImpl ::  CalendarImpl
orthodoxCalendarImpl = CalendarImpl isOrthodoxEaster getOrthodoxEasterMonday isOrthodoxWeekend

-- | Is the given day in the year an Easter Monday according to the Western Calendar?
isWesternEaster :: Int -> Year -> Bool
isWesternEaster = checkIsEasterMonday westernEasterMondays

-- | get easter monday from western calendar 
getWesternEasterMonday :: Year -> Int
getWesternEasterMonday = getEasterMonday westernEasterMondays

isWesternWeekend :: DayName -> Bool
isWesternWeekend Saturday = True
isWesternWeekend Sunday   = True
isWesternWeekend _        = False

-- | Is the given day in the year an Easter Monday according to the Orthodox Calendar?
isOrthodoxEaster :: Int -> Year -> Bool
isOrthodoxEaster = checkIsEasterMonday orthodoxEasterMondays

-- | get easter monday from orthodox calendar 
getOrthodoxEasterMonday :: Year -> Int
getOrthodoxEasterMonday = getEasterMonday orthodoxEasterMondays

isOrthodoxWeekend :: DayName -> Bool
isOrthodoxWeekend Saturday = True
isOrthodoxWeekend Sunday   = True
isOrthodoxWeekend _        = False

-- | checks to see if the specified day and year correspond to an easter monday in the array of easter 
checkIsEasterMonday :: Array Int Int -> Int -> Year -> Bool
checkIsEasterMonday easterMondays day year = day == (easterMondays ! (year - 1900))

-- | get day of the year easter monday falls on, given a particular calendar of easter mondays
getEasterMonday :: Array Int Int -> Year -> Int
getEasterMonday easterMondays year = easterMondays ! (year - 1900)

-- | make a calendar for a specific country
makeCalendar :: String -> (Calendar -> Date -> Bool) -> (Date -> Bool)
                    -> (Date -> Bool) -> CalendarImpl -> Calendar
makeCalendar name bus wkend hols calimpl = Calendar name bus wkend hols calimpl

-- main functions over calendars

isBusinessDay :: Calendar -> Date -> Bool
isBusinessDay cal = (calendarIsBusinessDay cal) cal

isWeekend :: Calendar -> Date -> Bool
isWeekend cal = calendarIsWeekend cal

isHoliday :: Calendar -> Date -> Bool
isHoliday cal = calendarIsHoliday cal

-- | Western calendar Easter  - taken from Calendar.cpp in Quantlib

westernEasterMondaysLst :: [Int]
westernEasterMondaysLst
    = [
                  98,  90, 103,  95, 114, 106,  91, 111, 102,   -- 1901-1909
             87, 107,  99,  83, 103,  95, 115,  99,  91, 111,   -- 1910-1919
             96,  87, 107,  92, 112, 103,  95, 108, 100,  91,   -- 1920-1929
            111,  96,  88, 107,  92, 112, 104,  88, 108, 100,   -- 1930-1939
             85, 104,  96, 116, 101,  92, 112,  97,  89, 108,   -- 1940-1949
            100,  85, 105,  96, 109, 101,  93, 112,  97,  89,   -- 1950-1959
            109,  93, 113, 105,  90, 109, 101,  86, 106,  97,   -- 1960-1969
             89, 102,  94, 113, 105,  90, 110, 101,  86, 106,   -- 1970-1979
             98, 110, 102,  94, 114,  98,  90, 110,  95,  86,   -- 1980-1989
            106,  91, 111, 102,  94, 107,  99,  90, 103,  95,   -- 1990-1999
            115, 106,  91, 111, 103,  87, 107,  99,  84, 103,   -- 2000-2009
             95, 115, 100,  91, 111,  96,  88, 107,  92, 112,   -- 2010-2019
            104,  95, 108, 100,  92, 111,  96,  88, 108,  92,   -- 2020-2029
            112, 104,  89, 108, 100,  85, 105,  96, 116, 101,   -- 2030-2039
             93, 112,  97,  89, 109, 100,  85, 105,  97, 109,   -- 2040-2049
            101,  93, 113,  97,  89, 109,  94, 113, 105,  90,   -- 2050-2059
            110, 101,  86, 106,  98,  89, 102,  94, 114, 105,   -- 2060-2069
             90, 110, 102,  86, 106,  98, 111, 102,  94, 114,   -- 2070-2079
             99,  90, 110,  95,  87, 106,  91, 111, 103,  94,   -- 2080-2089
            107,  99,  91, 103,  95, 115, 107,  91, 111, 103,   -- 2090-2099
             88, 108, 100,  85, 105,  96, 109, 101,  93, 112,   -- 2100-2109
             97,  89, 109,  93, 113, 105,  90, 109, 101,  86,   -- 2110-2119
            106,  97,  89, 102,  94, 113, 105,  90, 110, 101,   -- 2120-2129
             86, 106,  98, 110, 102,  94, 114,  98,  90, 110,   -- 2130-2139
             95,  86, 106,  91, 111, 102,  94, 107,  99,  90,   -- 2140-2149
            103,  95, 115, 106,  91, 111, 103,  87, 107,  99,   -- 2150-2159
             84, 103,  95, 115, 100,  91, 111,  96,  88, 107,   -- 2160-2169
             92, 112, 104,  95, 108, 100,  92, 111,  96,  88,   -- 2170-2179
            108,  92, 112, 104,  89, 108, 100,  85, 105,  96,   -- 2180-2189
            116, 101,  93, 112,  97,  89, 109, 100,  85, 105    -- 2190-2199
      ]

-- | Orthodox calendar Easter Mondays  - taken from Calendar.cpp in Quantlib

orthodoxEasterMondaysLst :: [Int]
orthodoxEasterMondaysLst
    = [
                 105, 118, 110, 102, 121, 106, 126, 118, 102,   -- 1901-1909
            122, 114,  99, 118, 110,  95, 115, 106, 126, 111,   -- 1910-1919
            103, 122, 107,  99, 119, 110, 123, 115, 107, 126,   -- 1920-1929
            111, 103, 123, 107,  99, 119, 104, 123, 115, 100,   -- 1930-1939
            120, 111,  96, 116, 108, 127, 112, 104, 124, 115,   -- 1940-1949
            100, 120, 112,  96, 116, 108, 128, 112, 104, 124,   -- 1950-1959
            109, 100, 120, 105, 125, 116, 101, 121, 113, 104,   -- 1960-1969
            117, 109, 101, 120, 105, 125, 117, 101, 121, 113,   -- 1970-1979
             98, 117, 109, 129, 114, 105, 125, 110, 102, 121,   -- 1980-1989
            106,  98, 118, 109, 122, 114, 106, 118, 110, 102,   -- 1990-1999
            122, 106, 126, 118, 103, 122, 114,  99, 119, 110,   -- 2000-2009
             95, 115, 107, 126, 111, 103, 123, 107,  99, 119,   -- 2010-2019
            111, 123, 115, 107, 127, 111, 103, 123, 108,  99,   -- 2020-2029
            119, 104, 124, 115, 100, 120, 112,  96, 116, 108,   -- 2030-2039
            128, 112, 104, 124, 116, 100, 120, 112,  97, 116,   -- 2040-2049
            108, 128, 113, 104, 124, 109, 101, 120, 105, 125,   -- 2050-2059
            117, 101, 121, 113, 105, 117, 109, 101, 121, 105,   -- 2060-2069
            125, 110, 102, 121, 113,  98, 118, 109, 129, 114,   -- 2070-2079
            106, 125, 110, 102, 122, 106,  98, 118, 110, 122,   -- 2080-2089
            114,  99, 119, 110, 102, 115, 107, 126, 118, 103,   -- 2090-2099
            123, 115, 100, 120, 112,  96, 116, 108, 128, 112,   -- 2100-2109
            104, 124, 109, 100, 120, 105, 125, 116, 108, 121,   -- 2110-2119
            113, 104, 124, 109, 101, 120, 105, 125, 117, 101,   -- 2120-2129
            121, 113,  98, 117, 109, 129, 114, 105, 125, 110,   -- 2130-2139
            102, 121, 113,  98, 118, 109, 129, 114, 106, 125,   -- 2140-2149
            110, 102, 122, 106, 126, 118, 103, 122, 114,  99,   -- 2150-2159
            119, 110, 102, 115, 107, 126, 111, 103, 123, 114,   -- 2160-2169
             99, 119, 111, 130, 115, 107, 127, 111, 103, 123,   -- 2170-2179
            108,  99, 119, 104, 124, 115, 100, 120, 112, 103,   -- 2180-2189
            116, 108, 128, 119, 104, 124, 116, 100, 120, 112    -- 2190-2199
      ]
      
      
-- | Builds the array of days representing the position of Easter Monday in the Western calendar each
--  year.  The year 1901 is at position 1 in the array (to facilitate arithmetic, such
--  as (year - 1900) to calculate array indices.
westernEasterMondays :: Array Int Int
westernEasterMondays
    = array (1,299) (zip [1..] westernEasterMondaysLst)

-- | Builds the array of days representing the position of Easter Monday in the Orthodox calendar
--  each year.  The year 1901 is at position 1 in the array (to facilitate arithmetic, such
--  as (year - 1900) to calculate array indices.
orthodoxEasterMondays :: Array Int Int
orthodoxEasterMondays
    = array (1,299) (zip [1..] orthodoxEasterMondaysLst)
    
