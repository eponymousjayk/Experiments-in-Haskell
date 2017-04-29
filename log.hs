logBase' b a
-- to match the inbuilt logarithm eaactly:
 |  (b,a) == (0,0) ||
    (b,a) == (1,1) ||
    b < 0     = 0/0
 |  b == 0    = -0
-- identities of logarithm:
 |  a == 1    = 0
 |  a == 0    = -1/0
 |  a <  1    = -(logBase' b (1/a))
 |  a <  b
 || b <= 1    = 1 / (logBase' a b)
 |  otherwise = 1 + (logBase' b (a/b))


log2 = logBase' 2

e = 1 + 1 + 1/2 + 1/6 + 1/24 + 1/120 + 1/720 + 1/5040 + 1/40320
ln' = logBase' e
