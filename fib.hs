fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib2 = (!!) fibs
