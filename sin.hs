cos' th
 | th < 0                  = cos'(-th)
 | otherwise               = 1 - 2*(sin'(th/2))**2

sin' th
 | th < 0                  = -sin'(-th)
 | th < 1e-24              = th
 | otherwise               = -4*(sub**3) + 3*sub
                             where sub = sin'(th/3)

tan' th = (sin'(th))/(cos'(th))
cot' th = 1/(tan' th)
sec' th = 1/(cos'(th))
csc' th = 1/(sin'(th))
