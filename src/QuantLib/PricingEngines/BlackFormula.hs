module QuantLib.PricingEngines.BlackFormula
        ( blackFormulaImpliedStdDev
        ) where

import           Data.Maybe
import           Numeric.SpecFunctions (erf)
import           QuantLib.Options

blackFormulaImpliedStdDev :: OptionType->Double->Double->Double->Double->Double->Maybe Double->Double->Int->Maybe Double
blackFormulaImpliedStdDev opType strike forward blackPrice discount displacement guess accuracy maxIter
        | blackPrice < 0.0      = Nothing
        | discount  <= 0.0      = Nothing
        | strike     < 0.0      = Nothing
        | forward   <= 0.0      = Nothing
        | displacement < 0.0    = Nothing
        | otherwise             = impliedStdDevNewton residualAndVega accuracy realGuess maxIter
        where   realGuess               = max positiveFloor (fromMaybe apprGuess guess)
                apprGuess               = blackFormulaImpliedStdDevApproximation opType strike forward blackPrice discount displacement
                residualAndVega         = blackImpliedStdDevHelper opType strike forward blackPrice displacement

-- | Small positive floor keeping Newton iterates in the valid domain.
positiveFloor :: Double
positiveFloor = 1.0e-12

-- | Newton iteration on the implied standard deviation. The objective is
-- monotone increasing (vega is positive), steps are clamped to a positive
-- floor, and a bisection fallback kicks in when a step stalls. Returns
-- Nothing when the requested accuracy is not reached within maxIter steps.
impliedStdDevNewton :: (Double-> (Double, Double))-> Double-> Double-> Int-> Maybe Double
impliedStdDevNewton f accuracy x0 maxIter = go (f x0) x0 Nothing 0
        where   go (r, dv) x bracket iter
                        | abs r <= accuracy       = Just x
                        | iter >= maxIter         = Nothing
                        | stalled                 = go (f mid) mid Nothing (iter + 1)
                        | otherwise               = go nxt x' bracket' (iter + 1)
                        where   nxt               = f x'
                                x'                = max (x - r / dv) positiveFloor
                                r'                = fst nxt
                                stalled           = abs r' >= abs r && isJust bracket
                                mid               = case bracket of
                                                        Just (a, b) -> 0.5 * (a + b)
                                                        Nothing     -> x'
                                bracket'
                                        | r * r' < 0.0    = Just (min x x', max x x')
                                        | otherwise       = bracket

{-# ANN blackImpliedStdDevHelper "NoHerbie" #-}
-- | Residual (model price minus target price) and vega of the Black formula
-- for a given standard deviation. The vega is option-type independent:
-- dPrice/dStdDev = (forward + displacement) * pdf(d1).
blackImpliedStdDevHelper :: OptionType-> Double-> Double-> Double-> Double-> Double-> (Double, Double)
blackImpliedStdDevHelper opType strike forward blackPrice displacement x = (result - blackPrice, vega)
        where   result          = signedForward * cdf signedD1 - signedStrike * cdf signedD2
                vega            = (forward + displacement) * pdf signedD1
                signedD1        = d + temp
                signedD2        = d - temp
                d               = signedMoneyness/x
                temp            = intOpType * 0.5 * x
                intOpType       = toDouble opType
                signedMoneyness = intOpType*log ((forward+displacement)/(strike+displacement))
                signedForward   = intOpType*(forward+displacement)
                signedStrike    = intOpType*(strike +displacement)

cdf ::  Double -> Double
cdf x = 0.5 * (1 + erf (x / sqrt 2))

pdf ::  Double -> Double
pdf x = exp (-0.5 * x * x) / sqrt (2.0 * pi)

blackFormulaImpliedStdDevApproximation :: OptionType-> Double-> Double-> Double-> Double-> Double-> Double
blackFormulaImpliedStdDevApproximation opType strike forward blackPrice discount displacement
        | realStrike == realForward     = blackPrice/discount*sqrt2pi/realForward
        | otherwise     = if stdDev < 0.0 then 0.0 else stdDev
        where   realForward     = forward + displacement
                realStrike      = strike  + displacement
                intOpType       = toDouble opType
                moneynessDelta  = intOpType * (forward-strike)
                moneynessDeltaPi= moneynessDelta**2/pi
                temp1           = blackPrice/discount - moneynessDelta/2.0
                temp2           = temp1**2 - moneynessDeltaPi
                temp3           = if temp2<0.0 then 0.0 else sqrt temp2
                temp            = sqrt2pi*(temp1 + temp3)
                sqrt2pi         = sqrt (2.0*pi)
                stdDev          = temp/(realForward + realStrike)
