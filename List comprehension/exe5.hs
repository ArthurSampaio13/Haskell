pitag :: Int -> [(Int, Int, Int)]
pitag n = [(x,y,z) | z <- [1..n], 
                     x <- [1..n], 
                     y <- [1..n], 
                     x*x + y*y == z*z]