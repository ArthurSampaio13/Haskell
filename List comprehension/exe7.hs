comp1 :: [(Integer, Integer)]
comp1 =  concat [[(x,y) | y <- [3, 4]] | x <- [1,2]]