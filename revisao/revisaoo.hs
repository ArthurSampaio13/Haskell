import Data.List (delete)
{-
Questao 1
Implemente a função reverso :: [a] -> [a], que inverte a ordem dos elementos de uma lista. OBS: Não é permitido usar a função reverse da biblioteca padrão.
Ex: reverso "abc"
Main> "cba"
-}

reverso :: [a] -> [a]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

{- 
Questao 2
Crie uma função que converta um quantidade de segundos em uma String no formato "HH:MM:SS". O valor máximo possível a ser recebido por essa função é 359999 ("99:59:59").
Ex: converter 86399 = "23:59:59"
-}

converter :: Int -> String
converter n = show horas ++ ":" ++ show minutos ++ ":" ++ show segundos
                where
                    horas = div n 3600
                    restoHoras = mod n 3600
                    minutos = div restoHoras 60
                    segundos = mod restoHoras 60

{- 
Questao 3
Implemente uma função chamada rotateLeft :: Int -> [a] -> [a], que rotaciona uma lista para a esquerda uma quantidade n de vezes de acordo com o primeiro parâmetro da função.
Ex: rotateLeft 2 "abcd"
Main> "cdab"  
-}

rotateLeft :: Int -> [a] -> [a]
rotateLeft _ [] = []
rotateLeft n l = drop n l ++ take n l

{-
Questao 4
Crie uma função que remove o menor elemento de uma lista. Caso este elemento apareça mais de uma vez na lista, remova apenas a primeira ocorrência.
Ex: removeMin [4,5,6,4,7] = [5,6,4,7]
-}

removeMin :: Ord a => [a] -> [a]
removeMin l = delete (minimum l) l 

{-
Questao 5
Crie uma função (inclua a definição do seu tipo) que recebe uma lista de funções e um segundo argu-mento. Esta função devolve uma lista com todas as funções que ao serem aplicadas ao argumento re-tornam False.
Ex: escolheFuncoes [even, odd, (\x -> x `mod` 3 == 0)] 6 = [odd]
Use mapF se quiser testar as funcoes que saíram como resultado
-}

--func :: [(a -> b)] -> [a] -> [(a -> b)]
--func func x = [f | f <- func, f x == False]
