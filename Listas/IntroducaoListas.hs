--Introdução, listas, definição de funções.
-- Questao 1
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.List (intersect)
{-# HLINT ignore "Redundant bracket" #-}
iguais :: Int -> Int -> Int -> String
iguais a b c | a == b && b == c = "Todos sao iguais"
             | a == b && b /= c = "Dois sao iguais e um diferente"
             | a /= b && b == c = "Dois sao iguais e um diferente"
             | a /= b && b /= c && a /= c = "Todos sao diferentes"
             | otherwise = "Erro"

-- Questao 2
maioresMedia :: (Ord a, Fractional a) => a -> a -> a -> Int
maioresMedia a b c = (if a > media then 1 else 0) +
                     (if b > media then 1 else 0) +
                     (if c > media then 1 else 0)
                    where
                        media = (a+b+c) / 3
-- Questao 3
potencia :: Int -> Int
potencia x = x * x

-- Questao 4
potencia4 :: Int -> Int
potencia4 x = potencia x * potencia x

-- Questao 5
ouExclusivo :: Bool -> Bool -> Bool
ouExclusivo a b = (a || b) && not (a && b)

-- Questao 6
xMaior a b c | delta > 0 = max ((-b + sqrt(delta)) / 2 * a) ((-b - sqrt(delta)) / 2 * a)
             | otherwise = error "Nao ha valores"
                where
                    delta = b * b - 4 * a * c

xMenor a b c | delta > 0 = min ((-b + sqrt(delta)) / 2 * a) ((-b - sqrt(delta)) / 2 * a)
             | otherwise = error "Nao ha valores"
                where
                    delta = b * b - 4 * a * c

-- Questao 7
somaComLimites :: Int -> Int -> Int
somaComLimites n1 n2 = sum [n1 .. n2]

somaSemLimites :: Int -> Int -> Int
somaSemLimites n1 n2 = sum [n1 + 1 .. n2 - 1] 

-- Questao 8
encontrarMultiplos :: Int -> Int -> Int -> [Int]
encontrarMultiplos n1 n2 n3 = 0 : [x | x <- [n1 .. n2], x `mod` n3 == 0]

-- Questao 9
multiComSoma :: Int -> Int -> Int
multiComSoma a b = sum $ replicate a b

-- Questao 10
meuMod :: Integral a => a -> a -> a
meuMod a b = a - (divisao * b) 
                where 
                    divisao = a `div` b


-- Questao 11
sequencia :: (Eq t, Floating a, Num t) => t -> a
sequencia 1 = sqrt 6
sequencia n = sqrt(6 + sequencia (n-1))

-- Questao 12
maneiras :: Integral a => a -> a -> a
maneiras n p | n >= n = product [n .. 1] `div` product [nMenosP .. 1]
                        where
                            nMenosP = n - p

-- Questao 13
getPos :: (Eq t, Num a) => [t] -> t -> a
getPos (x:xs) elem | x == elem = 0
                   | otherwise = 1 + getPos xs elem

tupla :: (Num b, Ord t) => [t] -> [(t, b)]
tupla xs = zip [maior] [getPos xs maior]
            where 
                maior = maximum xs

-- Questao 14  
dic10 = map traduzir

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
           | otherwise = error "Valorr invalido"

-- Questao 15
delPosicaoN :: [Int] -> Int -> [Int]
delPosicaoN xs pos = take pos xs ++ drop (pos + 1) xs

-- Questao 16
inserirPosicaoX :: [Int] -> Int -> Int -> [Int]
inserirPosicaoX xs pos elem =  take pos xs ++ [elem] ++ drop pos xs

-- Questao 17
retornarValor :: [a] -> Int -> a
retornarValor xs pos = xs !! pos

-- Questao 18
juntarLista xs ys = xs ++ ys

-- Questao 19
listaNRepetida xs ys = xs `intersect` ys