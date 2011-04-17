{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}
module Main where

import Control.Monad
import qualified Data.Map as M
import QuantLib.Methods.MonteCarlo
import QuantLib.Stochastic
import GSL.Random.Gen
import Data.List

data MaxMinClosePricer = MMCP {
        mmcpHigh        :: Double,
        mmcpLow         :: Double,
        mmcpClose       :: Double
        } deriving (Show)

instance PathPricer MaxMinClosePricer where
        ppPrice _ path = MMCP high low close
                where   !close   = last xs
                        !high    = maximum xs
                        !low     = minimum xs
                        xs      = map getX path

data HistoSummary = HS (M.Map Double Int)
        deriving (Show)

addOnePath :: HistoSummary->MaxMinClosePricer->HistoSummary
addOnePath (HS m) (MMCP _ _ close) = HS newM
        where   (_, !newM) = M.insertLookupWithKey inserter roundedClose 1 m
                !roundedClose =  ((fromIntegral . round) (close*10000))/10000
                inserter _ new_value old_value = old_value+new_value

instance Summary HistoSummary MaxMinClosePricer where
        sNorm _ _ = 0.0 -- we don't care about convergence now
        sSummarize m paths = foldl' addOnePath m paths  

printMap :: HistoSummary->IO ()
printMap (HS m) = do
        forM_ list printPlain
        where
                printPlain (a, b) = do 
                        putStrLn $ (show a)++","++(show b)
                list    = M.toList m

getHsSize (HS m) = M.size m

main :: IO ()
main = do
        let summary = HS M.empty
        let mmcp    = MMCP 0.0 0.0 0.0
        let start   = Dot 0.0 1.0
        let sp      = GeometricBrownian 0.0 1.0
        let discrete= Euler 0.01
        rng <- mkNormalGen
        let pg      = ProcessGenerator start 100 sp rng discrete
        let pmc     = PathMonteCarlo summary mmcp pg
        s <- monteCarlo pmc 100000
        putStrLn $ show $ getHsSize s 
