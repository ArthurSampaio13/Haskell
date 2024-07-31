{--
Questão 1
Implemente a função interseccao :: Eq a => [a] -> [a] -> [a] que encontra a interseção de duas listas.
--}

interseccao :: Eq a => [a] -> [a] -> [a]
interseccao xs ys = [x | x <- xs, x `elem` ys]

{--
Questão 2
Crie uma função uniao :: Eq a => [a] -> [a] -> [a] que une duas listas sem duplicar elementos.
--}

uniao :: Eq a => [a] -> [a] -> [a]
uniao xs ys = xs ++ [y | y <- ys, y `notElem` xs]

{--
Questão 3
Implemente uma função zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] que funciona como a função zipWith da biblioteca padrão.
--}
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

{--
Questão 4
Defina uma função elementosUnicos :: Eq a => [a] -> [a] que retorna uma lista contendo apenas os elementos únicos da lista original.
--}

elementosUnicos :: Eq a => [a] -> [a]
elementosUnicos [] = []
elementosUnicos (x:xs) | x `elem` xs = elementosUnicos xs
                       | otherwise = x : elementosUnicos xs

{--
Questão 5
Implemente a função fibonacci :: Int -> Int que retorna o n-ésimo número da sequência de Fibonacci.
--}

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

{--
Questão 7
Implemente uma função insereOrdenado :: Ord a => a -> [a] -> [a] que insere um elemento em uma lista ordenada mantendo a ordem.
--}

insereOrdenado :: Ord a => a -> [a] -> [a]
insereOrdenado elem (x:xs) | elem <= x = elem : x : xs
                           | otherwise =  x : insereOrdenado elem xs

{--
Questão 9
Implemente uma função rotacionaDireita :: Int -> [a] -> [a] 
que rotaciona uma lista para a direita uma quantidade n de vezes.
--}

rotacionaDireita :: Int -> [a] -> [a] 
rotacionaDireita n l = drop n l ++ take n l

{--
Questão 10
Crie uma função somaPares :: [(Int, Int)] -> [Int] que soma os elementos de cada par em uma lista de pares.
--}
somaPares :: [(Int, Int)] -> [Int]
somaPares = map (uncurry (+))

{--
Questão 11
Implemente a função compacta :: Eq a => [a] -> [(a, Int)] 
que agrupa elementos consecutivos iguais em tuplas contendo o elemento e a quantidade de vezes que ele se repete.
--}

compacta :: Eq a => [a] -> [(a, Int)] 
compacta [] = []
compacta (x:xs) = (x, length (takeWhile (==x) (x:xs))) : compacta (dropWhile (==x) (x:xs))

{--
Questão 12
Crie uma função descompacta :: [(a, Int)] -> [a] que converte uma lista de tuplas (elemento, quantidade) de volta para uma lista normal.
--}

descompacta :: [(a, Int)] -> [a]
descompacta [] = []
descompacta ((x, n):xs) = replicate n x ++ descompacta xs
