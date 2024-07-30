--Questao 1
data ListaAninhada a = Elem a | Lista [ListaAninhada a]

planificar :: ListaAninhada a -> [a]
planificar (Elem a) = [a]
planificar (Lista a) = concatMap planificar a

planificar' :: ListaAninhada a -> [a]
planificar' (Elem a) = [a]
planificar' (Lista []) = []
planificar' (Lista (x:xs)) = planificar' x ++ planificar' (Lista xs)

-- Questao 2
data Expr = Lit Int | Soma Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

avalia :: Expr -> Int
avalia (Lit a) = a
avalia (Soma e1 e2) = avalia e1 + avalia e2
avalia (Sub e1 e2) = avalia e1 - avalia e2
avalia (Mul e1 e2) = avalia e1 * avalia e2
avalia (Div e1 e2) = avalia e1 `div` avalia e2

-- Questao 3
data Arvore a = Folha | No (Arvore a) a (Arvore a)

repeat' :: a -> Arvore a
repeat' x = No (repeat' x) x (repeat' x)

takeTree :: (Eq t, Num t) => t -> Arvore a -> Arvore a
takeTree 0 _ = Folha
takeTree _ Folha = Folha
takeTree n (No a1 v a2) = No (takeTree (n-1) a1) v (takeTree (n-1) a2)

replicateTree :: (Eq t, Num t) => t -> a -> Arvore a
replicateTree n = takeTree n . repeat'

-- Questao 4
resolveEq a b c | delta == 0 = (Just $ (negate b / 2 * a), Just $ (negate b / 2 * a))
                | delta > 0 = (Just $ (negate b + sqrt delta / 2 * a), Just $ (negate b - sqrt delta / 2 * a))
                | otherwise = (Nothing, Nothing)
                    where 
                        delta = b * b - 4 * a * c