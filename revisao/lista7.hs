import Distribution.Simple (UserHooks(preBench))
import Data.ByteString (split)
{--
Questão 1
Defina um tipo Tree a que representa uma árvore binária. 
Implemente uma função height :: Tree a -> Int que calcula a altura da árvore.
--}

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

height :: Tree a -> Int
height Empty = 0
height (Node _ esq dir) = 1 + max (height esq) (height dir)

{--
Questão 2
Implemente uma função inOrder :: Tree a -> [a] que retorna a lista dos elementos da árvore em ordem.
--}

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a esq dir) = inOrder esq ++ [a] ++ inOrder dir 

{--
Questão 3
Crie uma função mirror :: Tree a -> Tree a que inverte uma árvore binária.
--}

mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Node a esq dir) = Node a dir esq

{--
Questão 4
Defina uma função leafCount :: Tree a -> Int que conta o número de folhas em uma árvore.
--}

leafCount :: Tree a -> Int 
leafCount Empty = 0
leafCount (Node _ Empty Empty) = 1
leafCount (Node a esq dir) = leafCount esq + leafCount dir

{--
Questão 5
Implemente uma função mapTree :: (a -> b) -> Tree a -> Tree b que aplica uma função a todos os elementos da árvore.
--}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty = Empty
mapTree f (Node b esq dir) = Node (f b) (mapTree f esq) (mapTree f dir)

{--
Questão 6
Crie uma função preOrder :: Tree a -> [a] que retorna a lista dos elementos da árvore em pré-ordem.
--}

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a esq dir) = [a] ++ preOrder esq ++ preOrder dir

{--
Questão 7
Implemente uma função postOrder :: Tree a -> [a] que retorna a lista dos elementos da árvore em pós-ordem.
--}

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a esq dir) = preOrder esq ++ preOrder dir ++ [a]

{--
Questão 8
Defina uma função treeElem :: Eq a => a -> Tree a -> Bool que verifica se um elemento está presente na árvore.
--}

treeElem :: Eq a => a -> Tree a -> Bool
treeElem _ Empty = False
treeElem elem (Node a esq dir) = elem == a || treeElem elem esq || treeElem elem dir

{--
Questão 9
Crie uma função treeToList :: Tree a -> [a] que converte uma árvore binária em uma lista usando percurso em largura.
--}

treeToList :: Tree a -> [a]
treeToList Empty = []
treeToList (Node a esq dir) = [a] ++ treeToList esq ++ treeToList dir

{--
Questão 10
Implemente uma função fromList :: [a] -> Tree a que cria uma árvore binária balanceada a partir de uma lista.
--}

fromList :: [a] -> Tree a
fromList [] = Empty
fromList xs = Node a (fromList esq) (fromList dir)
                where
                    (esq, a:dir) = splitAt (length xs `div` 2) xs