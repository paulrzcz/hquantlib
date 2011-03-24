module Tests.DayCounterTest(module Tests.DayCounterTest) where

import Date
import DayCounter
import Test.HUnit

daycountertests = TestList [TestLabel "Daycounter test1" daycounter_test1,
                            TestLabel "Daycounter test2" daycounter_test2]

daycounter_test1 = TestCase (assertEqual "actual365fixed day count" expected actual)
    where actual   = dayCount actual365fixedDayCounter testdate1 testdate4
          expected = 31
          
daycounter_test2 = TestCase (assertBool "actual365fixed year fraction" (expected == actual))
    where actual = yearFraction actual365fixedDayCounter testdate1 testdate4 testdate2 testdate3
          expected = (31.0 / 365.0)

-- test data

testdate1 = makeDate 02 07 2010
testdate2 = makeDate 10 07 2010
testdate3 = makeDate 19 07 2010
testdate4 = makeDate 02 08 2010

-- test 1
