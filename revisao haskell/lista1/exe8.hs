{--
Dados dois números n1 e n2, encontrar os múltiplos de n3 que se encontram nesse intervalo (inclusivo).
--}
multiplos :: Int -> Int -> Int -> [Int]
multiplos n1 n2 n3 = 0 : [x | x <- [n1 .. n2], mod x n3 == 0]