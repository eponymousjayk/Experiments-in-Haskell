(*^) x z
 | z < 0         = 1 / (x *^ (-z))
 | z < 0.0000001 = 1
 | otherwise     = sub * sub
                   where sub = x *^ (z/2)

