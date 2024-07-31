import Language.Haskell.TH (safe)
{--
Questão 1
Defina uma função altura :: Arvore a -> Int que calcula a altura de uma árvore.
--}

data Arvore a = Folha | No (Arvore a) a (Arvore a) deriving Show

altura :: Arvore a -> Int
altura Folha = 0
altura (No esq meio dir) = 1 + max (altura esq) (altura dir)

{--
Questão 2
Implemente uma função contaFolhas :: Arvore a -> Int que conta o número de folhas em uma árvore.
--}

contaFolhas :: Arvore a -> Int
contaFolhas Folha = 1
contaFolhas (No esq meio dir) = contaFolhas esq + contaFolhas dir

{--
Questão 3
Escreva uma função somaArvore :: Num a => Arvore a -> a que soma todos os elementos de uma árvore.
--}

somaArvore :: Num a => Arvore a -> a
somaArvore Folha = 0
somaArvore (No esq meio dir) = meio + somaArvore esq + somaArvore dir

{--
Questão 4
Defina uma função espelho :: Arvore a -> Arvore a que inverte a árvore (espelha os nós).
--}

espelho :: Arvore a -> Arvore a 
espelho Folha = Folha
espelho (No esq meio dir) = No dir meio esq

{--
Questão 5
Implemente uma função busca :: Eq a => a -> Arvore a -> Bool que verifica se um elemento está presente na árvore.
--}

busca :: Eq a => a -> Arvore a -> Bool
busca _ Folha = False
busca elem (No esq meio dir) = elem == meio || busca elem esq || busca elem dir

{--
Questão 6
Escreva uma função mapTree :: (a -> b) -> Arvore a -> Arvore b que aplica uma função a todos os elementos de uma árvore.
--}

mapTree :: (a -> b) -> Arvore a -> Arvore b
mapTree _ Folha = Folha
mapTree f (No esq meio dir) = No (mapTree f esq) (f meio) (mapTree f dir)

{--
Questão 7
Defina uma função niveles :: Arvore a -> [[a]] que retorna uma lista de listas, onde cada lista interna representa os elementos de um nível da árvore.
--}

niveles :: Arvore a -> [[a]]
niveles Folha = []
niveles (No esq v dir) = [v] : mergeNiveles (niveles esq) (niveles dir)
  where
    mergeNiveles [] ys = ys
    mergeNiveles xs [] = xs
    mergeNiveles (x:xs) (y:ys) = (x ++ y) : mergeNiveles xs ys

{--
Questão 8
Implemente uma função balanceada :: Arvore a -> Bool que verifica se uma árvore é balanceada (a diferença de altura entre as subárvores de qualquer nó não é maior que 1).
--}

balanceada :: Arvore a -> Bool
balanceada Folha = True
balanceada (No esq meio dir) = contaFolhas esq == contaFolhas dir

{--
Questão 9
Escreva uma função inOrder :: Arvore a -> [a] que retorna a lista de elementos de uma árvore em ordem.
--}

inOrder :: Arvore a -> [a]
inOrder Folha = []
inOrder (No esq meio dir) = inOrder esq ++ [meio] ++ inOrder dir

{--
Questão 10
Defina uma função minArvore :: Ord a => Arvore a -> Maybe a que encontra o menor elemento em uma árvore de busca binária.
--}

minArvore :: Ord a => Arvore a -> Maybe a
minArvore Folha = Nothing
minArvore (No Folha v _) = Just v
minArvore (No esq meio dir) = Just $ minimum $ inOrder (No esq meio dir)