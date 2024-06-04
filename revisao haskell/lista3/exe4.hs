{--
Defina a função euclides :: Int -> Int -> Int 
que implementa o algoritmo de Euclides para calcular o máximo divisor comum de dois inteiros não-negativos: 
se dois números são iguais, este número é o resultado; caso contrário, 
o menor número é subtraído do maior e o processo é repetido passando este novo número e o menor valor passado anteriormente como argumento. 
Exemplo:
> euclides 6 27
3
--}

euclides :: Int -> Int -> Int
euclides 0 _ = 0
euclides _ 0 = 0
euclides n m | n < 0 || m < 0 = error "Valores nao podem ser negativos"
             | n == m = m
             | n > m = n - euclides (n - 1) (m - 1)
             | otherwise = m - euclides (n-1) (m - 1)