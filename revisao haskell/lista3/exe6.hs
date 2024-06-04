import System.Posix (modificationTimeHiRes)
{--
Definir a função recursiva merge :: Ord a => [a] -> [a] -> [a] que une duas listas ordenadas em uma lista ordenada. 
Exemplo:
> merge [2,5,6] [1,3,4]
[1,2,3,4,5,6]
--}

merge' _ [] = []
merge' [] _ = []
merge' (x:xs) (y:ys) | x < y = x : y : merge' xs ys
                     | otherwise = y : x : merge' xs ys