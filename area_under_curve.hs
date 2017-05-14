approx dx f begin end
 | begin > end = 0
 | dx <= 0     = 1/0
 | otherwise   = (approx dx f (begin + dx) end) + (f (begin))*dx

integrate = approx 0.00001
integrateFrom0 = flip integrate 0

sin' = (integrateFrom0 cos)
-- less clean because the integral of sin is -cos, and cos(0) is 1
cos' = (\x -> 1 - x) . (integrateFrom0 sin)

slopeapprox xi f x = (f(x + xi) - f(x - xi))/(2*xi)

derivative = slopeapprox 0.000000000001

cos'' = derivative sin
sin'' = integrateFrom0 cos''

step x
 | x <= 0  = 0
 | x >  0  = 1

delta = derivative step
