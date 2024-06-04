{--
Escrever duas funções, x_maior que retorne o maior e x_menor que retorne o menor valor real, das raízes de uma equação do segundo grau. 
--}

xMaior a b c  | delta > 0 =  max ((-b + sqrt delta) / (2 * a))  ((-b - sqrt delta) / (2 * a))
              | otherwise = error "Sem raizes reais"
                            where
                                delta = b^2 - 4 * a *c 

xMenor a b c  | delta > 0 =  min ((-b + sqrt delta) / (2 * a))  ((-b - sqrt delta) / (2 * a))
              | otherwise = error "Sem raizes reais"
                            where
                                delta = b^2 - 4 * a *c 