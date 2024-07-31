import Text.XHtml (treeColors)
import Control.Monad (when)
{--
Questão 1
Defina um tipo Tree a que representa uma árvore binária. 
Implemente uma função countNodes :: Tree a -> Int que conta o número de nós na árvore.
--}

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Node a esq dir) = 1 + countNodes esq + countNodes dir

{--
Questão 2
Implemente uma função sumTree :: Num a => Tree a -> a que calcula a soma de todos os elementos da árvore.
--}

sumTree :: Num a => Tree a -> a
sumTree Empty = 0
sumTree (Node a esq dir) = a + sumTree esq + sumTree dir

{--
Questão 3
Crie uma função findMax :: Ord a => Tree a -> Maybe a que encontra o maior elemento na árvore.
--}

findMax :: Ord a => Tree a -> Maybe a
findMax Empty = Nothing
findMax (Node a esq dir) = Just $ maximum $ treeToList (Node a esq dir)
                            where
                                treeToList :: Tree a -> [a]
                                treeToList Empty = []
                                treeToList (Node a esq dir) = [a] ++ treeToList esq ++ treeToList dir

{--
Questão 5
Implemente uma função insertTree :: Ord a => a -> Tree a -> Tree a que insere um elemento em uma árvore de busca binária.
--}

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree x Empty = Node x Empty Empty
insertTree elem (Node a esq dir) | elem < a = Node a (insertTree elem esq) dir  
                                 | otherwise = Node a esq (insertTree elem dir) 

{--
Questão 6
Crie uma função isBalanced :: Tree a -> Bool que verifica se uma árvore binária está balanceada.
--}

isBalanced :: Tree a -> Bool
isBalanced Empty = True
isBalanced (Node a esq dir) = countNodes esq == countNodes dir

{--
Questão 7
Defina uma função flattenTree :: Tree a -> [a] que converte uma árvore binária em uma lista usando percurso em pré-ordem.
--}

flattenTree :: Tree a -> [a]
flattenTree Empty = []
flattenTree (Node a esq dir) = [a] ++ flattenTree esq ++ flattenTree dir

{--
Questão 8
Implemente uma função prune :: Int -> Tree a -> Tree a que remove todos os nós que estão a uma profundidade maior que a especificada.
--}

prune :: Int -> Tree a -> Tree a
prune _ Empty = Empty
prune 0 _ = Empty
prune p (Node a esq dir) = Node a (prune (p-1) esq) (prune (p-1) dir)

{--
Questão 9
Crie uma função paths :: Tree a -> [[a]] que retorna todas as listas de caminhos da raiz até as folhas.
--}

paths :: Tree a -> [[a]]
paths Empty = []
paths (Node a Empty Empty) = [[a]]
paths (Node a esq dir) = map (a:) (paths esq ++ paths dir)

