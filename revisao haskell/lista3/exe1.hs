{--
Como a versão recursiva da função fatorial se comporta se dermos a ela como argumento um número negativo? 
Modifique a implementação clássica para não permitir números negativos adicionando uma guarda ao passo recursivo.
--}

fatorial 0 = 1
fatorial n | n < 0 = error "Valores nao podem ser negativos"
           | otherwise = n * fatorial (n - 1)