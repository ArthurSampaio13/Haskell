{--
Questão 1
Implemente uma função intercalar :: [a] -> [a] -> [a] que recebe duas listas e retorna uma lista com os elementos intercalados. 
Se uma lista for maior que a outra, os elementos restantes devem ser adicionados ao final da lista resultante.
--}

intercalar :: [a] -> [a] -> [a]
intercalar _ [] = []
intercalar [] _ = []
intercalar (x:xs) (y:ys) = x : y : intercalar xs ys

{--
Questão 2
Implemente uma função splitHalf :: [a] -> ([a], [a]) que divide uma lista ao meio e retorna duas listas. 
Se a lista tiver comprimento ímpar, a primeira lista deve conter um elemento a mais.
--}

splitHalf :: [a] -> ([a], [a])
splitHalf [] = ([], [])
splitHalf l = splitAt ((length l + 1) `div` 2) l

{--
Questão 3
Crie uma função isPalindromo :: Eq a => [a] -> Bool que verifica se uma lista é um palíndromo, 
ou seja, se a lista é igual à sua reversa.
--}

isPalindromo :: Eq a => [a] -> Bool
isPalindromo l = l == reverse l

{--
Questão 4
Implemente uma função removerDuplicados :: Eq a => [a] -> [a] que remove elementos duplicados de uma lista, 
mantendo apenas a primeira ocorrência de cada elemento.
--}
removerDuplicados :: Eq a => [a] -> [a]
removerDuplicados [] = []
removerDuplicados (x:xs) = x : removerDuplicados (filter (/= x) xs)

{--
Questão 5
Crie uma função insereOrdenado :: Ord a => a -> [a] -> [a] que insere um elemento em uma lista já ordenada, mantendo a ordem.
--}

insereOrdenado :: Ord a => a -> [a] -> [a]
insereOrdenado _ [] = []
insereOrdenado elem (x:xs) | elem <= x = elem : x : xs
                           | otherwise = x : insereOrdenado elem xs

{--
Questão 6
Implemente uma função somaElementos :: Num a => [a] -> a que soma todos os elementos de uma lista.
--}

somaElementos :: Num a => [a] -> a
somaElementos = sum

{--
Questão 8
Implemente uma função removeImpares :: Integral a => [a] -> [a] que remove todos os números ímpares de uma lista.
--}

removeImpares :: Integral a => [a] -> [a]
removeImpares = filter even

removeImpares' :: Integral a => [a] -> [a]
removeImpares' (x:xs) = [x | x <- xs, even x]

{--
Questão 9
Crie uma função concatMap' :: (a -> [b]) -> [a] -> [b] que funciona como a função concatMap padrão, mas implementada por você.
--}
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' _ [] = []
concatMap' f (x:xs) = f x ++ concatMap' f xs

{--
Questão 10
Implemente uma função mapeiaArvore :: (a -> b) -> Arvore a -> Arvore b que aplica uma função a todos os elementos de uma árvore binária.
--}
data Arvore a = Folha | No (Arvore a) a (Arvore a) deriving Show

mapeiaArvore :: (a -> b) -> Arvore a -> Arvore b
mapeiaArvore _ Folha = Folha
mapeiaArvore f (No esq a dir) = No (mapeiaArvore f esq) (f a) (mapeiaArvore f dir)