{--
Construa uma função del_posicao_n :: [Int] -> Int -> [Int] 
em que dada uma lista de inteiros e a posição de um elemento qualquer, 
retorne uma nova lista sem aquele elemento da n-ésima posição.
--}

delPosicaoN (x:xs) pos = take (pos-1) (x:xs) ++ drop pos (x:xs)