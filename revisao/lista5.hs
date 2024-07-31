{--
Questão 1
Defina um tipo Pair a que represente um par de valores do mesmo tipo. Crie uma função swap :: Pair a -> Pair a que troca os elementos do par.
--}

data Pair a = Pair a a

swap :: Pair a -> Pair a
swap (Pair x y) = swap (Pair y x) 

{--
Questão 2
Defina um tipo Tree a para representar uma árvore binária com valores do tipo a. 
Implemente uma função treeSize :: Tree a -> Int que conta o número de nós em uma árvore.
--}

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ esq dir) = 1 + treeSize esq + treeSize dir

{--
Questão 3
Crie um tipo Shape que pode ser Circle com um raio ou Rectangle com largura e altura. 
Implemente uma função area :: Shape -> Float que calcula a área de uma forma geométrica.
--}

data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle float) = 3.14 * float * float
area (Rectangle a b) = a * b

{--
Questão 4
Defina um tipo Result que pode ser Success com um valor ou Error com uma mensagem de erro. 
Implemente uma função isSuccess :: Result a -> Bool que verifica se um Result é um Success.
--}

data Result a = Sucess a | Error a  

isSuccess :: Result a -> Bool
isSuccess (Sucess a) = True
isSuccess (Error a) = False

{--
Questão 5
Crie um tipo MaybeList a que pode ser NothingList ou JustList [a]. 
Implemente uma função lengthMaybeList :: MaybeList a -> Int que retorna o comprimento da lista, ou 0 se for NothingList.
--}

data MaybeList a = NothingList | JustList [a]

lengthMaybeList :: MaybeList a -> Int
lengthMaybeList (JustList a) = length a
lengthMaybeList NothingList = 0

{--
Questão 6
Defina um tipo Direction que pode ser North, South, East ou West.
Implemente uma função opposite :: Direction -> Direction que retorna a direção oposta.
--}

data Direction = North | South | East | West deriving Show

opposite :: Direction -> Direction
opposite North =  South
opposite South = North
opposite East = West
opposite West = East

{--
Questão 7
Crie um tipo Expr para representar expressões aritméticas com números inteiros e operações de soma e multiplicação. 
Implemente uma função eval :: Expr -> Int que avalia uma expressão.
--}

data Expr = Num Int | Add Expr Expr | Mul Expr Expr

eval :: Expr -> Int
eval (Num a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

{--
Questão 8
Defina um tipo Person que contém um nome, uma idade e uma cidade. 
Implemente uma função showPerson :: Person -> String que retorna uma string formatada com as informações da pessoa.
--}

data Person = Person String Int String

showPerson :: Person -> String
showPerson (Person nome idade cidade) = "Name: " ++ nome ++ ", " ++ "Age: " ++ show idade ++ ", " ++ "City: " ++ cidade

{--
Questão 9
Crie um tipo Tree a para representar uma árvore binária e uma função insert :: Ord a => a -> Tree a -> Tree a que insere um valor na árvore mantendo a ordem.
--}

insert :: Ord a => a -> Tree a -> Tree a
insert valor Empty = Node valor Empty Empty
insert valor (Node a esq dir) | valor < a = Node valor (insert a esq) dir
                              | otherwise = Node valor esq (insert valor dir)

{--
Questão 10
Defina um tipo List a que pode ser EmptyList ou Cons a (List a). 
Implemente uma função toList :: List a -> [a] que converte uma List a para uma lista padrão de Haskell.
--}

data List a = EmptyList | Cons a (List a)
toList :: List a -> [a]
toList EmptyList = []
toList (Cons x xs) = x : toList xs