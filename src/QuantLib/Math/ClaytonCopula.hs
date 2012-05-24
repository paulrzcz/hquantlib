
{-|  Original code and algorithm from the Quantlib project
     implemented in Haskell by Nicholas Pezolano 
                                  npezolano "at" gmail.com
-}
module QuantLib.Math.ClaytonCopula
        ( claytonCopula
        ) where

claytonCopula :: Double -> Double -> Double -> Maybe Double
claytonCopula x y theta
	| x >= 0.0 
	|| x <= 1.0 
	|| y >= 0.0 
	|| y <=1.0 
	|| theta /=0 
	|| theta >= -1.0
	 = Nothing

	| otherwise 
	= Just $ max( (x ** (-theta) ) +   (y ** (-theta)-1.0)  **   (-1.0/theta)) 0
	
