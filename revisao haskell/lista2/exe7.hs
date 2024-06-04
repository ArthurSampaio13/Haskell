{--
Mostre que a compreensão de lista [(x,y) | x <- [1,	2], y <- [3,4]], com dois geradores, 
pode ser representada usando duas compreensões de lista, cada uma com apenas um gerador. 
Dica: Procure usar a função concat. 
--}
dois = concat [[(x,y) | y <- [3, 4]] | x <- [1,2]]