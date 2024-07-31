data ListaAninhada a = Elem a | Lista [ListaAninhada a]

planificar :: ListaAninhada a -> [a]
planificar (Elem a) = [a]
planificar (Lista a) = concatMap planificar a

data Arvore a = Folha | No (Arvore a) a (Arvore a)

repeat' :: a -> Arvore a
repeat' x = No (repeat' x) x (repeat' x)