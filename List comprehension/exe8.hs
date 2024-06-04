posicoes :: Eq a => a -> [a] -> [Int]
posicoes x xs = buscar x (zip xs [0..])

buscar :: Eq a => a -> [(a,b)] -> [b]
buscar k xs = [v | (k', v) <- xs, k == k']