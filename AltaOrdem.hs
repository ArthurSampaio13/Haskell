-- Questao 1
-- quicksort
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort menores ++ [x] ++ quicksort maiores
    where
        menores = [a | a <- xs, a < x]
        maiores = [a | a <- xs, a >= x]

impares :: [Int] -> [Int]
impares (x:xs) = filter odd $ quicksort (x:xs)

-- Questao 2
posicao :: Int -> [a] -> a
posicao pos xs = xs !! pos

-- Questao 3 
repete :: Int -> [[Int]]
repete valor | valor == 0 = []
             | otherwise = [take valor (repeat valor)] ++ repete (valor - 1)

-- Questao 4
palindromo :: Eq a => [a] -> Bool
palindromo xs | xs == reverse xs = True
              | otherwise = False

-- Questao 5
fibonacci :: Int -> [Int]
fibonacci 0 = []
fibonacci 1 = [0]
fibonacci 2 = [0, 1]
fibonacci n = 0 : 1 : zipWith (+) (tail (fibonacci (n-1))) (fibonacci (n-1))

-- Questao 6
--a)
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (x:xs) | not (p x) = False
              | otherwise = all' p xs

--b)
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs) | p x = True
              | otherwise = any' p xs

--c)
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = takeWhile' p xs

--d)
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x : dropWhile' p xs

-- Questao 7
map' :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
map' f = foldr (\x xs -> f x : xs) []

filter' :: Foldable t => (a -> Bool) -> t a -> [a]
filter' f = foldr (\x xs -> if f x then x : xs else xs) []

-- Questao 8
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

-- Questao 9
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x
       | p x = []
       | otherwise = h x : unfold p h t (t x)

int2bin :: Integer -> [Integer]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

map'' :: (b -> a) -> [b] -> [a]
map'' p = unfold null (p . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' = unfold (const False) id

-- Questao 10
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap p _ [x] = [p x]
altMap p f (x:y:xs) = p x : f y : altMap p f xs
