-- Something to do with the reimann zeta function.

epsilon = 0.000000000001

zeta' x = sum $ takeWhile (>0) $ map (\y -> if (1/(y**x) < epsilon) then 0 else (1/(y**x))) [1,2..]


-- from the haskell wiki
-- https://wiki.haskell.org/Gamma_and_Beta_function

cof = [76.18009172947146,-86.50532032941677,24.01409824083091,-1.231739572450155,0.001208650973866179,-0.000005395239384953]

ser = 1.000000000190015

gammaln xx = let tmp' = (xx+5.5) - (xx+0.5)*log(xx+5.5)
                 ser' = ser + sum (zipWith (/) cof [xx+1..])
             in -tmp' + log(2.5066282746310005 * ser' / xx)

gamma = exp . gammaln
