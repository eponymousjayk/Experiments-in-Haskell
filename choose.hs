n `c` 1 = n
n `c` k
 | k == n ||
   k == 0    = 1
 | otherwise = ((n - 1) `c` (k - 1)) + ((n - 1) `c` k)
