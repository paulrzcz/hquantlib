-- | Golden values for blackFormulaImpliedStdDev, recorded from the
-- GSL-backed implementation (hmatrix-gsl root DNewton) with discount 1.0,
-- no guess, accuracy 1.0e-6 and maxIter 100, immediately before its removal.
module Tests.Golden
        ( GoldenRow(..)
        , goldenRows
        ) where

import QuantLib.Options

data GoldenRow = GoldenRow
        { grOpType      :: OptionType
        , grForward     :: Double
        , grStrike      :: Double
        , grBlackPrice  :: Double
        , grDisplacement:: Double
        , grExpected    :: Maybe Double
        } deriving (Show, Eq)

goldenRows :: [GoldenRow]
goldenRows =        [ gr Call 100.0 80.0 20.03991434342184 0.0 (Just 0.10000000005101141)
        , gr Call 100.0 80.0 23.53439010317375 0.0 (Just 0.3000000023916819)
        , gr Call 100.0 80.0 29.462965797650575 0.0 (Just 0.5000000000127807)
        , gr Call 100.0 90.0 10.712380896073682 0.0 (Just 0.10000000042794911)
        , gr Call 100.0 90.0 17.0128799018497 0.0 (Just 0.30000000000000177)
        , gr Call 100.0 90.0 24.16000295519605 0.0 (Just 0.4999999999999934)
        , gr Call 100.0 100.0 3.987761167674492 0.0 (Just 9.999999998758577e-2)
        , gr Call 100.0 100.0 11.923538474048499 0.0 (Just 0.29999999999999943)
        , gr Call 100.0 100.0 19.741265136584744 0.0 (Just 0.4999999999999155)
        , gr Call 100.0 110.0 0.9539473918572199 0.0 (Just 0.1000000000001869)
        , gr Call 100.0 110.0 8.141012048964214 0.0 (Just 0.3000000000000096)
        , gr Call 100.0 110.0 16.095681194570233 0.0 (Just 0.5000000000000138)
        , gr Call 100.0 120.0 0.1473322632569607 0.0 (Just 0.1000000000005465)
        , gr Call 100.0 120.0 5.440563467814318 0.0 (Just 0.3000000000516114)
        , gr Call 100.0 120.0 13.108974138557578 0.0 (Just 0.5000000000009133)
        , gr Put 100.0 80.0 1.185929513210425 0.0 (Just 0.20000000000043078)
        , gr Put 100.0 80.0 6.391183524514428 0.0 (Just 0.40000000006107544)
        , gr Put 100.0 90.0 3.5891081160548026 0.0 (Just 0.20000000000070298)
        , gr Put 100.0 90.0 10.57127549346388 0.0 (Just 0.40000000000000907)
        , gr Put 100.0 100.0 7.965567455405804 0.0 (Just 0.19999999726450082)
        , gr Put 100.0 100.0 15.851941887820608 0.0 (Just 0.3999999999999746)
        , gr Put 100.0 110.0 14.292010941409899 0.0 (Just 0.20000000000013612)
        , gr Put 100.0 110.0 22.108139117843983 0.0 (Just 0.39999999999999847)
        , gr Put 100.0 120.0 22.14729881057815 0.0 (Just 0.20000000000002438)
        , gr Put 100.0 120.0 29.188094709522503 0.0 (Just 0.40000000000429314)
        , gr Call 3.0e-2 4.0e-2 5.929647566052104e-4 1.0e-2 (Just 0.20000040139234856)
        , gr Put 3.0e-2 2.0e-2 9.291235590323391e-4 1.0e-2 (Just 0.300001211877909)
        ]

-- | Shorthand for GoldenRow used in goldenRows.
gr :: OptionType-> Double-> Double-> Double-> Double-> Maybe Double-> GoldenRow
gr = GoldenRow
