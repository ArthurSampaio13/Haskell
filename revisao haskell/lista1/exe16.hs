{--
Construa uma função inserir_posicao_x :: [Int] -> Int -> Int -> [Int] em que, 
dada uma lista de inteiros, uma posição e um elemento a ser inserido, 
retorne uma nova lista com aquele elemento na n-ésima posição.

--}
inserirPosicaoX :: [Int] -> Int -> Int -> [Int]
inserirPosicaoX (x:xs) pos elem = take pos (x:xs) ++ [elem] ++ drop pos (x:xs)