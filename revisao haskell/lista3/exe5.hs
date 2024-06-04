{--
Defina as funções abaixo usando recursão:

a) Decidir se todos os valores em uma lista são True:
and :: [Bool] -> Bool

b) Concatenar uma lista de listas:
concat :: [[a]] -> [a]

c) Produzir uma lista com n elementos idênticos:
replicate :: Int -> a -> [a]

d) Selecionar o n-ésimo elemento em uma lista:
(!!) :: [a] -> Int -> a

e) Decidir se um valor está presente em uma lista:
elem :: Eq a => a -> [a] -> Bool
--}

--a)
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

--b)
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

--c)
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n m = m : replicate' (n-1) m

--d)
procurar :: [a] -> Int -> a
procurar (x:xs) pos | pos == 0 = x
                    | otherwise = procurar xs (pos - 1)

--e)
elem' :: Eq a => a -> [a] -> Bool
elem' v (x:xs) | v == x = True
               | v /= x = elem' v xs
               | otherwise = False