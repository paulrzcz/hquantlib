module QuantLib.Math.Copulas
        ( Copula,
          Copulas (..) 
        ) where

{-| Copula type class. 
 -| Normally instance should implement only copulaFunc.
 -| Method copula provides a precheck for [0..1] range for x and y but real implementation is in copulaFunc
 -}
class Copula a where
        copula :: a -> Double -> Double -> Maybe Double
        copula t = precheckRange (copulaFunc t)
        copulaFunc :: a -> Double -> Double -> Maybe Double

-- Copula must be in [0,1] range
precheckRange :: (Double->Double->Maybe Double) -> Double -> Double -> Maybe Double
precheckRange f x y
        | x >= 0.0 || x <= 1.0
        || y>= 0.0 || y <= 1.0
                = f x y
        | otherwise
                = Nothing

{-| Copula data types with parameters required by the concrete copula definition
 -}
data Copulas = ClaytonCopula Double
        | MinCopula
        | MaxCopula
        | AliMikhailHaqCopula Double
        | FarlieGumbelMorgensternCopula Double
        | FrankCopula Double

instance Copula Copulas where
        copulaFunc (ClaytonCopula theta) = claytonCopula theta
        copulaFunc MinCopula = minCopula
        copulaFunc MaxCopula = maxCopula
        copulaFunc (AliMikhailHaqCopula theta) = aliMikhailHaqCopula theta
        copulaFunc (FarlieGumbelMorgensternCopula theta) = farlieGumbelMorgenstern theta
        copulaFunc (FrankCopula theta) = frankCopula theta

{- Private implementations   -}

aliMikhailHaqCopula :: (Fractional a, Ord a) => a -> a -> a -> Maybe a
aliMikhailHaqCopula theta x y
        | theta >= -1.0 && theta <= 1.0
                = Just ((x*y)/(1.0 - theta * (1.0-x)*(1.0-y)))
        | otherwise
                = Nothing

farlieGumbelMorgenstern :: (Fractional a, Ord a) => a -> a -> a -> Maybe a
farlieGumbelMorgenstern theta x y
        | theta >= -1.0 && theta <= 1.0
                = Just (x*y + theta*x*y*(1.0-x)*(1.0-y))
        | otherwise
                = Nothing

{-|  Original code and algorithm from the Quantlib project
     implemented in Haskell by Nicholas Pezolano 
                                  npezolano "at" gmail.com
-}
claytonCopula :: Double -> Double -> Double -> Maybe Double
claytonCopula theta x y
	|  theta ==0 
	|| theta < -1.0
	 = Nothing

	| otherwise 
	= Just $ max( (x ** (-theta) ) +   (y ** (-theta)-1.0)  **   (-1.0/theta)) 0
	
minCopula ::  Ord a => a -> a -> Maybe a
minCopula x y = Just (min x y)

maxCopula ::  (Fractional a, Ord a) => a -> a -> Maybe a
maxCopula x y = Just (max 0.0 (x+y-1.0))

frankCopula theta x y
    | theta ==0.0 = Nothing
    | otherwise = Just (-1.0/theta  *  log (1 + (exp (-theta*x) - 1.0) * (exp (-theta*y) -1.0) / (exp (-theta) - 1.0)   ))
  
