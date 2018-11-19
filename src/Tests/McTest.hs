{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import           Control.Monad
import           Data.List
import qualified Data.Map                    as M
import           QuantLib.Methods.MonteCarlo
import           QuantLib.Methods.Pricer     (MaxMinClosePricer (..))
import           QuantLib.Stochastic

newtype HistoSummary = HS (M.Map Double Int)
        deriving (Show)

toDouble :: Int -> Double
toDouble = fromIntegral

addOnePath :: HistoSummary->MaxMinClosePricer->HistoSummary
addOnePath (HS m) (MMCP _ _ close) = HS newM
        where   (_, !newM) = M.insertLookupWithKey inserter roundedClose 1 m
                !roundedClose =  toDouble (round (close*10000))/10000
                inserter _ new_value old_value = old_value+new_value

instance Summary HistoSummary MaxMinClosePricer where
        sNorm _ _   = 0.0 -- we don't care about convergence now
        sSummarize  = foldl' addOnePath

printMap :: HistoSummary->IO ()
printMap (HS m) = forM_ list printPlain
        where
                printPlain (a, b) = putStrLn $ show a ++ "," ++ show b
                list    = M.toList m

getHsSize :: HistoSummary -> Int
getHsSize (HS m) = M.size m

main :: IO ()
main = do
        let summary = HS M.empty
        let mmcp    = MMCP 0.0 0.0 0.0
        let start   = Dot 0.0 1.0
        let sp      = GeometricBrownian 0.0 0.005
        let discrete= Euler 0.01
        rng <- mkInverseNormal :: IO (InverseNormal PureMT)
        let pg      = ProcessGenerator start 1000 sp rng discrete
        let pmc     = PathMonteCarlo summary mmcp pg
        let s = monteCarlo pmc 1000
        printMap s
        print (getHsSize s)
