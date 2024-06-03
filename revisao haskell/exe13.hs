{--
Defina uma função que, dada uma lista numérica, retorne uma tupla, 
que contenha o maior da lista bem como sua posição relativa.
--}

percorrer (x:xs)     | x == maximum (x:xs) = 0
                     | otherwise = 1 + percorrer xs

retornar (x:xs) = (maximum (x:xs), percorrer (x:xs))
