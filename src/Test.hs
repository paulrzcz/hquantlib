{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Maybe
import Numeric.SpecFunctions (erf)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, assertBool, assertEqual, assertFailure)
import Test.QuickCheck
import QuantLib.Options
import QuantLib.PricingEngines.BlackFormula
import Tests.Golden

main :: IO ()
main = defaultMain tests

-- Reference undiscounted Black price with the same sign conventions as
-- blackImpliedStdDevHelper, used to synthesize and verify prices.
blackPriceRef :: OptionType-> Double-> Double-> Double-> Double-> Double
blackPriceRef opType strike forward x displacement
        = sgn*(forward+displacement)*cdfRef sd1 - sgn*(strike+displacement)*cdfRef sd2
        where   sgn     = toDouble opType
                d       = sgn*log ((forward+displacement)/(strike+displacement))/x
                sd1     = d + sgn*0.5*x
                sd2     = d - sgn*0.5*x

cdfRef :: Double -> Double
cdfRef x = 0.5 * (1 + erf (x / sqrt 2))

solve :: OptionType-> Double-> Double-> Double-> Double-> Maybe Double-> Maybe Double
solve opType strike forward displacement price guess
        = blackFormulaImpliedStdDev opType strike forward price 1.0 displacement guess 1.0e-6 100

testGoldenRow :: Int-> GoldenRow-> Test
testGoldenRow i row = testCase (show i) $ do
        let mi = solve (grOpType row) (grStrike row) (grForward row) (grDisplacement row) (grBlackPrice row) Nothing
        assertBool ("expected Just, got " ++ show mi) (isJust mi)
        let x' = fromJust mi
            residual = abs (blackPriceRef (grOpType row) (grStrike row) (grForward row) x' (grDisplacement row) - grBlackPrice row)
        assertBool ("residual " ++ show residual) (residual <= 1.0e-6)
        case grExpected row of
                Just e  -> assertBool ("golden mismatch: new " ++ show x' ++ ", golden " ++ show e) (abs (x' - e) <= 1.0e-4)
                Nothing -> assertBool "expected Nothing" (isNothing mi)

testGuards :: Test
testGuards = testCase "input guards" $ do
        assertBool "negative black price"  (isNothing (solve Call 100.0 100.0 0.0 (-1.0) Nothing))
        assertBool "zero discount"         (isNothing (blackFormulaImpliedStdDev Call 100.0 100.0 10.0 0.0 0.0 Nothing 1.0e-6 100))
        assertBool "negative strike"       (isNothing (solve Call (-5.0) 100.0 0.0 10.0 Nothing))
        assertBool "zero forward"          (isNothing (solve Call 100.0 0.0 0.0 10.0 Nothing))
        assertBool "negative displacement" (isNothing (solve Call 100.0 100.0 (-0.5) 10.0 Nothing))

testUnreachable :: Test
testUnreachable = testCase "unreachable price yields Nothing" $
        assertEqual "call price above undiscounted forward" Nothing
                (solve Call 80.0 100.0 0.0 150.0 Nothing)

testGuessIndependence :: Test
testGuessIndependence = testCase "result independent of guess" $
        case (solve Put 100.0 100.0 0.0 7.965567455405804 Nothing, solve Put 100.0 100.0 0.0 7.965567455405804 (Just 0.05)) of
                (Just a, Just b) -> assertBool ("diff " ++ show (a - b)) (abs (a - b) <= 1.0e-4)
                _ -> assertFailure "expected Just on both solves"

testCdfSpots :: Test
testCdfSpots = testCase "normal cdf spot values" $ do
        assertEqual "cdf 0" 0.5 (cdfRef 0.0)
        assertBool "cdf 1.96" (abs (cdfRef 1.96 - 0.9750021048517795) < 1.0e-9)

genInRange :: Double-> Double-> Gen Double
genInRange lo hi = choose (lo, hi)

propRoundTrip :: Property
propRoundTrip = forAll (elements [Call, Put]) $ \ot ->
        forAll (genInRange 10.0 500.0) $ \forward ->
        forAll (genInRange 0.85 1.18) $ \ratio ->
        forAll (genInRange 0.0 0.1) $ \dispFrac ->
        forAll (genInRange 0.08 1.5) $ \x ->
                let strike       = forward * ratio
                    displacement = dispFrac * forward
                    price        = blackPriceRef ot strike forward x displacement
                    mi           = solve ot strike forward displacement price Nothing
                in counterexample (show (ot, forward, strike, displacement, x, mi)) $
                        maybe False (\x' -> abs (x' - x) <= 1.0e-4) mi

tests :: [Test]
tests =
        [ testGroup "golden" $ zipWith testGoldenRow [1 :: Int ..] goldenRows
        , testGroup "cases"
            [ testGuards
            , testUnreachable
            , testGuessIndependence
            , testCdfSpots
            ]
        , testGroup "properties"
            [ testProperty "round trip from known std dev" propRoundTrip
            ]
        ]
