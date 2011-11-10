{-| The inverse normal cumulative distribution is a non-linear function for which no closed-form solution exists. The function is continuous, monotonically increasing, infinitely differentiable, and maps the open interval (0,1) to the whole real line. By <a href="http://home.online.no/~pjacklam/notes/invnorm/"> An algorithm for computing the inverse normal cumulative distribution function</a>
-}

module QuantLib.Math.InverseNormal
        ( inverseNormal
        ) where


a1 ::  Double
a1 = -3.969683028665376e+01;
a2 ::  Double
a2 =  2.209460984245205e+02;
a3 ::  Double
a3 = -2.759285104469687e+02;
a4 ::  Double
a4 =  1.383577518672690e+02;
a5 ::  Double
a5 = -3.066479806614716e+01;
a6 ::  Double
a6 =  2.506628277459239e+00;

b1 ::  Double
b1 = -5.447609879822406e+01;
b2 ::  Double
b2 =  1.615858368580409e+02;
b3 ::  Double
b3 = -1.556989798598866e+02;
b4 ::  Double
b4 =  6.680131188771972e+01;
b5 ::  Double
b5 = -1.328068155288572e+01;

c1 ::  Double
c1 = -7.784894002430293e-03;
c2 ::  Double
c2 = -3.223964580411365e-01;
c3 ::  Double
c3 = -2.400758277161838e+00;
c4 ::  Double
c4 = -2.549732539343734e+00;
c5 ::  Double
c5 =  4.374664141464968e+00;
c6 ::  Double
c6 =  2.938163982698783e+00;

d1 ::  Double
d1 =  7.784695709041462e-03;
d2 ::  Double
d2 =  3.224671290700398e-01;
d3 ::  Double
d3 =  2.445134137142996e+00;
d4 ::  Double
d4 =  3.754408661907416e+00;

-- Limits of the approximation regions (break-points)
xlow ::  Double
xlow = 0.02425;
xhigh ::  Double
xhigh = 1.0 - xlow;

-- Precision of Double at 1.0 point
ulp ::  Double
ulp = 2.220446049250313E-16

-- | Computes the inverse cumulative standard normal distribution N(0, 1)
inverseNormal ::  Double -> Double
inverseNormal x 
        | x < xlow      = inverseInLowerRegion z
        | x <= xhigh    = inverseInCentralRegion z
        | otherwise     = inverseInHigherRegion z
        where   z | x < 0.0 || x >1.0   = inverseRecovery x
                  | otherwise           = x


inverseRecovery :: Double -> Double
inverseRecovery x
        | isCloseToZero = 0.0
        | isCloseToOne  = 1.0
        | otherwise     = 0.0/0.0 -- NaN effectively
        where   isCloseToZero   = abs x < ulp
                isCloseToOne    = diff <= tolerance * abs x || diff <= tolerance
                diff            = abs (x-1.0)
                tolerance       = 42*ulp
{-# INLINE inverseRecovery #-}

inverseInLowerRegion ::  Double -> Double
inverseInLowerRegion x = (((((c1*z+c2)*z+c3)*z+c4)*z+c5)*z+c6) / ((((d1*z+d2)*z+d3)*z+d4)*z+1.0)
        where   z      = sqrt (-2.0*log x)
{-# INLINE inverseInLowerRegion #-}

inverseInCentralRegion ::  Double -> Double
inverseInCentralRegion x = (((((a1*r+a2)*r+a3)*r+a4)*r+a5)*r+a6)*z / (((((b1*r+b2)*r+b3)*r+b4)*r+b5)*r+1.0)
        where   r       = z*z
                z       = x - 0.5
{-# INLINE inverseInCentralRegion #-}

inverseInHigherRegion ::  Double -> Double
inverseInHigherRegion x =  -(((((c1*z+c2)*z+c3)*z+c4)*z+c5)*z+c6) / ((((d1*z+d2)*z+d3)*z+d4)*z+1.0)
        where   z       = sqrt (-2.0 * log (1.0 - x))
{-# INLINE inverseInHigherRegion #-}

