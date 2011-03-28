{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module QuantLib.Methods.MonteCarlo
        ( module QuantLib.Methods.MonteCarlo
        ) where

import Control.Monad
import QuantLib.Stochastic.Process
import QuantLib.Stochastic.Random

class Summary m where
        sSummarize      :: PathPricer p => m->[p]->m
        sNorm           :: m->m->Double

class PathGenerator m where
        pgGenerate      :: m->IO Path

class PathPricer m where
        ppPrice         :: m->Path->m

class (Summary m, PathGenerator m, PathPricer m) => MonteCarlo m where
        mcCalculate     :: m->Int->IO m

        mcCalculate mc size     = do 
                liftM (sSummarize mc) priced
                where   paths   = map (\_ -> pgGenerate mc) [1..size]
                        priced  = mapM (liftM (ppPrice mc)) paths

monteCarlo :: (Summary s, PathPricer p, PathGenerator g) => PathMonteCarlo s p g->Int->IO s
monteCarlo (PathMonteCarlo s p g) size = do
        liftM (sSummarize s) priced
        where   paths   = map (\_ -> pgGenerate g) [1..size]
                priced  = mapM (liftM (ppPrice p)) paths

data (Summary s, PathPricer p, PathGenerator g) => PathMonteCarlo s p g
        = PathMonteCarlo {
                pmcSummary      :: s,
                pmcPricer       :: p,
                pmcGenerator    :: g
                }

data LastPointPricer = LastPointPricer

instance PathPricer LastPointPricer where
        pgGenerate _ path = 

data (StochasticProcess sp, NormalGenerator b, Discretize d) => ProcessGenerator sp b d 
        = ProcessGenerator {
                pgStart         :: Dot,
                pgLength        :: Int,
                pgProcess       :: sp,
                pgGenerator     :: b,
                pgDiscretize    :: d
        }

instance (StochasticProcess sp, NormalGenerator b, Discretize d) => PathGenerator (ProcessGenerator sp b d) where
        pgGenerate (ProcessGenerator start len sp b d) = generatePath b d sp len start

