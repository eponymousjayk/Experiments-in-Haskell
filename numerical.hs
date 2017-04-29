derivative f = \x -> (f(x + 1e-4) - f(x))/1e-4

dn 0 f = f
dn n f = derivative (dn (n-1) f)

fac 0 = 1
fac n = product [1..n]

integral f x = sum $ map (\k -> (((dn k f) 0)*(x**(k+1)))/(fac (k+1))) [0..10]
