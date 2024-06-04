{--
Reutilizando a função potencia_2, construir uma função potencia_4 que retorne o seu argumento elevado à quarta potência.
--}
potencia2 :: Int -> Int
potencia2 x = x ^ 2

potencia4 :: Int -> Int
potencia4 x = potencia2 x ^ 2