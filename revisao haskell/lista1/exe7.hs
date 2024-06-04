{--
Criar funções que calculam a soma dos números entre n1 e n2, incluindo e excluindo os limites.

--}
somaInclusiva :: Int -> Int -> Int
somaInclusiva n1 n2 = sum [n1 .. n2]

somaExclusiva :: Int -> Int -> Int
somaExclusiva n1 n2 = sum [n1 + 1 .. n2 - 1]