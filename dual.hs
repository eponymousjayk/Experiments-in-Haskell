data Dual u = D (u,u)

instance (Show u) => Show (Dual u) where
   show (D(u,v)) = "(" ++ (show u) ++ " + " ++ (show v) ++ "e)"

instance (Num u) => Num (Dual u) where
   (+) (D (u,u')) (D (v,v')) = (D (u+v,u'+v'))
   (-) (D (u,u')) (D (v,v')) = (D (u-v,u'-v'))
   (*) (D (u,u')) (D (v,v')) = (D (u*v,u'*v + u*v'))
   abs (D (u,u')) = (D (abs u, u' * (signum u)))
   signum (D (u,u')) = (D (signum u, 0))
   fromInteger n = (D (fromInteger n,0))

instance (Fractional u) => Fractional (Dual u) where
   (/) (D (u,u')) (D (v,v')) = (D (u/v, (u'*v - u*v')/(v*v)))
   fromRational r = (D (fromRational r, 0))

instance (Eq u) => Eq (Dual u) where
   (==) (D (u,u')) (D (v,v')) = u == v

instance (Ord u) => Ord (Dual u) where
   (<=) (D (u,u')) (D (v,v')) = (u <= v)

instance (Floating u, Fractional u) => Floating (Dual u) where
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

getEpsilonCoefficient (D(u,v)) = v
derivative f = getEpsilonCoefficient . f . toDual

toDual x = D(x,1)
