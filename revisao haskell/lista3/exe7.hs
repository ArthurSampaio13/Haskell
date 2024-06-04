{--
Implemente recursivamente funções que:

a) calcule a soma de uma lista de inteiros;
b) obtenha o número de elementos de uma lista;
c) selecione o último elemento de uma lista não-vazia.
--}

somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

length' [] = 0
length' (x:xs) = 1 + length xs

ult [x] = [x]
ult (_:xs) = ult xs