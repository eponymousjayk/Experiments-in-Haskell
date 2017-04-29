pi' = (+) 3 $ sum $ take 100 $ zipWith (*) (map subr [2,4..])
                                           (map ((**) (-1)) [0,1..])
      where subr n = 4 / (n * (n + 1) * (n + 2))


