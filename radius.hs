approx_ r dr
 | r  <= 0   = 0
 | dr <= 0   = 1/0
 | otherwise = (approx_ (r-dr) dr) + 2*pi*r*dr
