module Test() where

import Test.HUnit
import Tests.DayCounterTest
import Tests.CalendarTest

runtests1 = runTestTT daycountertests
runtests2 = runTestTT calendartests

