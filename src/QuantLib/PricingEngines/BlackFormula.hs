module QuantLib.PricingEngines.BlackFormula
        ( blackFormulaImpliedStdDev
        ) where

import Data.Maybe
import QuantLib.Options
import Numeric.GSL.Root
import Numeric.GSL.Special.Erf

blackFormulaImpliedStdDev :: OptionType->Double->Double->Double->Double->Double->Maybe Double->Double->Int->Maybe Double
blackFormulaImpliedStdDev opType strike forward blackPrice discount displacement guess accuracy maxIter
        | blackPrice < 0.0      = Nothing
        | discount  <= 0.0      = Nothing
        | strike     < 0.0      = Nothing
        | forward   <= 0.0      = Nothing
        | displacement < 0.0    = Nothing
        | otherwise             = Just stdDev
        where
                realGuess               = fromMaybe apprGuess guess
                apprGuess               = blackFormulaImpliedStdDevApproximation opType strike forward blackPrice discount displacement
                blackFunction           = blackImpliedStdDevHelper opType strike forward blackPrice displacement
                ([stdDev], _)           = root DNewton accuracy maxIter blackFunction [realGuess]

blackImpliedStdDevHelper :: OptionType-> Double-> Double-> Double-> Double-> [Double]-> [Double]
blackImpliedStdDevHelper opType strike forward blackPrice displacement [x] =
        [max 0.0 result - blackPrice]
        where   result = signedForward * cdf signedD1 - signedStrike * cdf signedD2
                signedD1 = d + temp
                signedD2 = d - temp
                d        = signedMoneyness/x
                temp     = intOpType * 0.5 * x
                intOpType= toDouble opType
                signedMoneyness = intOpType*log ((forward+displacement)/(strike+displacement))
                signedForward = intOpType*(forward+displacement)
                signedStrike  = intOpType*(strike +displacement)

blackImpliedStdDevHelper _ _ _ _ _ _ = undefined

cdf ::  Double -> Double
cdf x = 0.5 * (1 + erf (x / sqrt 2))

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
