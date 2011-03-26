module QuantLib.Methods.MonteCarlo
        ( module QuantLib.Methods.MonteCarlo
        ) where

import Control.Monad
import QuantLib.Stochastic.Process

class Summary m where
        sSummarize      :: [m]->m
        sNorm           :: m->m->Double

class PathGenerator m where
        pgGenerate      :: m->IO Path

class PathPricer m where
        ppPrice         :: Path->m

class (Summary m, PathGenerator m, PathPricer m) => MonteCarlo m where
        mcCalculate     :: m->Int->IO m

        mcCalculate mc size     = do 
                liftM sSummarize priced
                where   paths   = map (\_ -> pgGenerate mc) [1..size]
                        priced  = mapM (liftM ppPrice) paths


