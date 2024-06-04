{--
Usando compreensão de listas e a função grid definida na questão anterior, defina uma função 
quadrado :: Int -> [(Int,Int)] que retorna um plano de coordenadas quadrado de tamanho n, 
excluindo a diagonal principal (0,0) a (n,n). 
Por exemplo:
> quadrado 2
[(0,1), (0,2), (1,0), (1,2), (2,0), (2,1)]

--}
grid :: Int -> Int -> [(Int,Int)]
grid a b = [(x,y) | x <- [0 .. a], y <- [0 .. b]]

quadrado a = [(x,y) | (x,y) <- grid a a, x /= y]