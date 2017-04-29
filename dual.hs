data Dual u = D (u,u) deriving(Show)

instance (Num a) => Num (Dual a) where
   (+) (D (u,u')) (D (v,v')) = (D (u+v,u'+v'))
   (-) (D (u,u')) (D (v,v')) = (D (u-v,u'-v'))
   (*) (D (u,u')) (D (v,v')) = (D (u*v,u'*v + u*v'))
   abs (D (u,u')) = (D (abs u, u' * (signum u)))
   signum (D (u,u')) = (D (signum u, signum u'))
   fromInteger n = (D (fromInteger n,0))

instance (Fractional a) => Fractional (Dual a) where
   (/) (D (u,u')) (D (v,v')) = (D (u/v, (u'*v - u*v')/(v*v)))
   fromRational r = (D (fromRational r, 0))

instance (Eq a) => Eq (Dual a) where
   (==) (D (u,u')) (D (v,v')) = u == v

instance (Ord a) => Ord (Dual a) where
   (<=) (D (u,u')) (D (v,v')) = (u <= v)

instance (Floating a, Fractional a) => Floating (Dual a) where
   pi = (D (pi,0))
   exp (D (u,u')) = (D (exp u, u' * exp (u)))
   log (D (u,u')) = (D (log u, u'/u))
   sin (D (u,u')) = (D (sin u, u' * cos (u)))
   cos (D (u,u')) = (D (cos u, (-u') * sin(u)))
   asin (D (u,u')) = (D (asin u, (u'/(sqrt(1 - u**2)))))
   acos (D (u,u')) = (D (acos u, ((-u')/(sqrt (1 - u**2)))))
   atan (D (u,u')) = (D (atan u, u'/(1 + u**2)))
   sinh (D (u,u')) = (D (sinh u, u'*(cosh u)))
   cosh (D (u,u')) = (D (cosh u, u'*(sinh u)))
   asinh (D (u,u')) = (D (asinh u, u'/(sqrt (u*u + 1))))
   acosh (D (u,u')) = (D (acosh u, u'/(sqrt (u*u - 1))))
   atanh (D (u,u')) = (D (atanh u, u'/(1 - u*u)))

derivative f x = getEpsilonCoefficient $ f (D(x,1))

getEpsilonCoefficient (D (u,v)) = v
