module Tests.CalendarTest(module Tests.CalendarTest) where

import Date
import Calendar
import CalendarUK
import Test.HUnit

calendartests = TestList [TestLabel "Calendar test1" calendar_test1,
                          TestLabel "Calendar test2" calendar_test2]

calendar_test1 = TestCase (assertEqual "UK Calendar isBusinessDay" expected actual)
    where actual   = isBusinessDay (calendarUK UKExchange) testdate1
          expected = True
 
calendar_test2 = TestCase (assertEqual "UK Calendar isBusinessDay" expected actual)
    where actual   = isBusinessDay (calendarUK UKExchange) testdate2
          expected = False
          
calendar_test3 = TestCase (assertEqual "UK Calendar isBusinessDay" expected actual)
    where actual   = isBusinessDay (calendarUK UKExchange) testdate3
          expected = False
          
          
          
-- test data

testdate1 = makeDate 02 07 2010
testdate2 = makeDate 10 07 2010
testdate3 = makeDate 25 04 2011 -- Easter Monday 2011
testdate4 = makeDate 06 04 2011 -- Easter Friday 2012
