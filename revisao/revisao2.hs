import Data.Tuple (swap)
{--
Questão 1
Implemente a função intercala :: [a] -> [a] -> [a], 
que intercala dois vetores de forma alternada. 
Caso um dos vetores termine antes do outro, a função deve continuar com os elementos restantes do vetor mais longo.
--}

intercala :: [a] -> [a] -> [a]
intercala (x:xs) (y:ys) = x : y : intercala xs ys

{--
Questão 2
Crie uma função fatorial :: Int -> Int que calcule o fatorial de um número de forma recursiva.
Ex: fatorial 5 = 120
--}

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

{--
Questão 3
Implemente uma função mapTree :: (a -> b) -> Arvore a -> Arvore b que aplique uma função a todos os elementos de uma árvore binária.
Ex: mapTree (*2) (No (No Folha 1 Folha) 2 (No Folha 3 Folha)) = No (No Folha 2 Folha) 4 (No Folha 6 Folha)
--}
data Arvore a = Folha | No (Arvore a) a (Arvore a) deriving Show

mapTree :: (a -> b) -> Arvore a -> Arvore b
mapTree f (No esq b dir) = No (mapTree f esq) (f b) (mapTree f dir)

{--
Questão 5
Implemente a função potencia :: Int -> Int -> Int, 
que calcule a potência de um número de forma recursiva.
Ex: potencia 2 3 = 8
--}

potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia n m = n * potencia n (m - 1)

{--
Questão 6
Crie uma função intersperse :: a -> [a] -> [a], que insere um elemento entre cada elemento de uma lista.
Ex: intersperse 0 [1,2,3,4] = [1,0,2,0,3,0,4]
--}

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse elem (x:xs) = x : elem : intersperse elem xs

{--
Questão 7
Implemente a função filterTree :: (a -> Bool) -> Arvore a -> Arvore a que filtre os elementos de uma árvore binária de acordo com um predicado.
Ex: filterTree (>2) (No (No Folha 1 Folha) 2 (No Folha 3 Folha)) = No Folha 3 Folha
--}

filterTree :: (a -> Bool) -> Arvore a -> Arvore a
filterTree f Folha = Folha
filterTree f (No esq a dir) | f a = No (filterTree f esq) a (filterTree f dir)
                            | otherwise = Folha

{--
Questão 8
Crie uma função zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c], 
que combina dois vetores aplicando uma função a cada par de elementos correspondentes.
Ex: zipWith' (+) [1,2,3] [4,5,6] = [5,7,9]
--}

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

{--
Questão 9
Implemente a função particiona :: (a -> Bool) -> [a] -> ([a], [a]) que separe uma lista em duas, com base em um predicado.
Ex: particiona even [1,2,3,4,5,6] = ([2,4,6], [1,3,5])
--}

particiona :: (a -> Bool) -> [a] -> ([a], [a])
particiona _ [] = ([], [])
particiona f (x:xs) | f x = (x : ys, zs)
                    | otherwise = (ys, x : zs)
                        where
                            (ys, zs) = particiona f xs

{--
Questão 10
Crie uma função primos :: Int -> [Int] que gere uma lista dos primeiros n números primos.
Ex: primos 5 = [2,3,5,7,11]
--}

fatores n = [x | x <- [1 .. n], mod n x == 0]
isPrime n = fatores n == [1,n]

primos :: Int -> [Int]
primos n = take n [x | x <- [1..], isPrime x]

{--
Questão 11
Implemente a função substElem :: Eq a => a -> a -> [a] -> [a] 
que substitua todas as ocorrências de um elemento por outro em uma lista.
Ex: substElem 3 9 [1,2,3,4,3,5] = [1,2,9,4,9,5]
--}

substElem :: Eq a => a -> a -> [a] -> [a] 
substElem _ _ [] = []
substElem elem1 elem2 (x:xs) | x == elem1 = elem2 : substElem elem1 elem2 xs
                             | otherwise = x : substElem elem1 elem2 xs

{--
Questão 12
Crie uma função replicate' :: Int -> a -> [a] 
que crie uma lista contendo n repetições de um elemento.
Ex: replicate' 3 'a' = "aaa"
--}

replicate' :: Int -> a -> [a] 
replicate' 0 _ = []
replicate' n a = a : replicate' (n - 1) a

{--
Questão 13
Implemente a função inverteTree :: Arvore a -> Arvore a que inverta os filhos de todos os nós de uma árvore binária.
Ex: inverteTree (No (No Folha 1 Folha) 2 (No Folha 3 Folha)) = No (No Folha 3 Folha) 2 (No Folha 1 Folha)
--}
inverteTree :: Arvore a -> Arvore a
inverteTree Folha = Folha
inverteTree (No esq a dir) = No (inverteTree dir) a (inverteTree esq)

{--
Questão 14
Crie uma função contaOcorrencias :: Eq a => a -> [a] -> Int que conte o número de ocorrências de um elemento em uma lista.
Ex: contaOcorrencias 3 [1,2,3,4,3,5,3] = 3
--}
contaOcorrencias :: Eq a => a -> [a] -> Int
contaOcorrencias _ [] = 0
contaOcorrencias n (x:xs) | x == n = 1 + contaOcorrencias n xs
                          | otherwise = contaOcorrencias n xs

{--
Questão 15
Implemente a função expandir :: Int -> [a] -> [a] que duplique cada elemento de uma lista n vezes.
Ex: expandir 2 [1,2,3] = [1,1,2,2,3,3]
--}

expandir :: Int -> [a] -> [a]
expandir _ [] = []
expandir n (x:xs) = replicate n x ++ expandir n xs