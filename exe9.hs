produto :: Num a => [a] -> [a] -> a
produto xs ys = sum [x * y | (x, y) <- zip xs ys]