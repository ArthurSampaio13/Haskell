{--
Defina uma função que converta uma lista de dígitos (unitários, 0 a 9) 
em uma outra lista, que é a sua tradução em String. 
Considere um dicionário do tipo:
dic_10 = [(0,"zero"), (1,"um"), (2,"dois"), …, (9,"nove")]
--}

traduzir x | x == 0 = (0,"zero")
           | x == 1 = (1, "Um")
           | x == 2 = (2, "Dois")
           | x == 3 = (3, "Tres")
           | x == 4 = (4, "Quatro")
           | x == 5 = (5, "Cinco")
           | x == 6 = (6, "Seis")
           | x == 7 = (7, "Sete")
           | x == 8 = (8, "Oito")
           | x == 9 = (9, "Nove")
           | otherwise = error "Valor invalido"

dic10 = map traduzir