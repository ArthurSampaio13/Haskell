import Foreign.C (eILSEQ)
{-
Questão 1
Implemente a função somaLista :: Num a => [a] -> a que retorna a soma de todos os elementos de uma lista.
-}

somaLista :: Num a => [a] -> a
somaLista = sum

{-
Questão 2
Crie uma função produtoLista :: Num a => [a] -> a que calcula o produto de todos os elementos de uma lista.
-}

produtoLista :: Num a => [a] -> a
produtoLista = product

{-
Questão 3 **
Defina um tipo de dado BinTree a representando uma árvore binária e implemente a função altura :: BinTree a -> Int que calcula a altura da árvore.
-}

data BinTree a = Vazia | Nodo a (BinTree a) (BinTree a) deriving Show
 
altura :: BinTree a -> Int
altura Vazia = 0
altura (Nodo _ b c) = 1 + max (altura b) (altura c)

{--
Questão 4
Implemente uma função maiorElemento :: Ord a => [a] -> a que retorna o maior elemento de uma lista.
--}

maiorElemento :: Ord a => [a] -> a
maiorElemento = maximum

{--
Questão 5
Crie uma função rotateRight :: Int -> [a] -> [a] que rotaciona uma lista para a direita uma quantidade n de vezes.
--}
rotateRight :: Int -> [a] -> [a]
rotateRight n l = drop n l ++ take n l

{--
Questão 6 **
Defina uma função ehPrimo :: Int -> Bool que verifica se um número é primo.
--}

primos n = [x | x <- [1 .. n], mod n x == 0]
ehPrimo n = primos n == [1, n]

{--
Questão 7 **
Implemente uma função permutacoes :: [a] -> [[a]] que gera todas as permutações de uma lista.
--}
permutacoes :: [a] -> [[a]]
permutacoes [] = [[]]
permutacoes xs = [x:ys | x <- xs, ys <- permutacoes (remove x xs)]
                    where
                        remove _ [] = []
                        rempove y (z:zs) | y == z = zs
                                         | otherwise = z : remove y zs

{--
Questão 8
Crie uma função intercalar :: [a] -> [a] -> [a] que intercala dois listas.
--}

intercalar :: Ord a => [a] -> [a] -> [a]
intercalar xs ys = quicksort $ (xs ++ ys)
                    where
                        quicksort [] = []
                        quicksort (p:xs) = quicksort lesser ++ [p] ++ quicksort greater
                            where
                                lesser = filter (< p) xs
                                greater = filter (>= p) xs

{--
Questão 9
Implemente uma função mapeia :: (a -> b) -> [a] -> [b] que aplica uma função a todos os elementos de uma lista (uma implementação da função map).
--}

mapeia :: (a -> b) -> [a] -> [b]
mapeia f (x:xs) = f x : mapeia f xs

{--
Questão 10
Defina uma função inverteArvore :: BinTree a -> BinTree a que inverte os filhos de cada nó de uma árvore binária.
--}

inverteArvore :: BinTree a -> BinTree a
inverteArvore Vazia = Vazia
inverteArvore (Nodo a esq dir) = Nodo a (inverteArvore dir) (inverteArvore esq)