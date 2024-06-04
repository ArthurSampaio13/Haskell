{--
Uma tupla (x,y,z) de inteiros positivos é Pitagoreana se satisfaz a equação x² + y² = z². 
Usando compreensão de listas com três geradores, defina a função pitag :: Int -> [(Int, Int, Int)] que 
retorna uma lista de todas as tuplas que satisfazem a condição estabelecida e cujos componentes são menores ou iguais a um dado limite. 
Exemplo: 
> pitag 10
[(3,4,5), (4,3,5), (6,8,10), (8,6,10)]
--}
pitag :: Int -> [(Int, Int, Int)]
pitag n = [(x,y,z) | x <- [1 .. n], 
                     y <- [1 .. n],
                     z <- [1 .. n],
                     x^2 + y^2 == z^2]