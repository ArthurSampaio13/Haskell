{--
Utilizando a função sum, faça uma função que calcule a multiplicação entre dois números quaisquer, considerando números positivos e negativos.
--}
multi :: Int -> Int -> Int
multi a b = sum $ replicate a b