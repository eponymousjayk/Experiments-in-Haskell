A bunch of haskell widgets using numerical techniques I found interesting.

dual.hs in particular uses dual numbers to calculate derivatives. Dual numbers are numbers of the form u + u'e, where e != 0 but e^2 = 2.

sin.hs implements sine and cosine recursively using small angle approximation as a base case, yielding answers close to the in-built sine and cosine.

area\_under\_curve.hs implements integration and derivatives by naive computational means, including a derivation of the dirac delta function, where d(0) = a very large number, from the step function.

log.hs implements the log function by recursion alone, and matches the in-built logarithm function exactly.
