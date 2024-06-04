{--
Implementar a fórmula que indica de quantas maneiras é possível escolher n 
objetos de uma coleção original de m objetos, onde m >= n.
--}
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

maneiras m n | m < n = 0
             | otherwise = fatorial m / (fatorial n * fatorial (m - n))