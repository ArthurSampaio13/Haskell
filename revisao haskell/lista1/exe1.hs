{--
Fornecidos três valores a, b e c, escreva uma função que retorne quantos dos três são iguais. 
A resposta pode ser 3 (todos iguais), 2 (dois iguais e o terceiro diferente) ou 0 (todos diferentes)
--}
comparar :: Int -> Int -> Int -> String
comparar a b c | a == b && b == c = "Todos os valores sao iguais"
               | a /= b && b /= c && a /= c = "Todos diferentes"
               | otherwise = "Dois iguais e o terceiro diferente"