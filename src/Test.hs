{-# LANGUAGE BangPatterns #-}
module Main where

import Test.Framework (Test)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding ( (==>) )

main :: IO () 
main = defaultMain tests

ignore :: Functor f => f a -> f () 
ignore = fmap (const ())

sumTest :: Int->Int->Int
sumTest x y = x + y

tests :: [Test]
tests = 
    [ testGroup "cases" $ zipWith (testCase . show) [1 :: Int ..] $
        [] 
    , testGroup "properties" $ zipWith (testProperty . show) [1 :: Int ..] $ 
        [ property $ \ a b -> sumTest a b == a + b
        ] 
    ]
