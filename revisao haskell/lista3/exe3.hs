{--
Defina o operador de exponenciação ^ utilizando uma função recursiva, 
semelhante ao padrão usado para implementar a multiplicação com o operador *:
(*) :: Num a => a -> a -> a
m * 0 = 0
m * n = m + (m * (n - 1))
--}

expo :: Int -> Int -> Int
expo _ 0 = 1
expo n e = n * expo n (e - 1)