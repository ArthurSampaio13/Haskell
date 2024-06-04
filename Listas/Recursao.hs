-- Questao 1
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import System.Posix (modificationTimeHiRes)
import System.FilePath.Posix (stripExtension)
import Data.ByteString (sort)
import Data.IntMap.Merge.Lazy (merge)
import Distribution.Simple.PackageIndex (SearchResult(None))
import GHC.List (errorEmptyList)
{-# HLINT ignore "Use foldr" #-}
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n | n > 0 = n * fatorial (n - 1)
           | otherwise = error "So valores positivos"

-- Questao 2
somar :: Int -> Int
somar 0 = 0
somar n = n + somar (n - 1)

-- Questao 3
expo :: Int -> Int -> Int
expo _ 0 = 1
expo n m = n * expo n (m-1)

-- Questao 4
euclides :: Int -> Int -> Int
euclides n m | n == m = n
             | n > m = euclides (n - m) m
             | otherwise = euclides n (m - n)

-- Questao 5
--a)
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x == and' xs

--b)
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) =  x ++ concat' xs

--c)
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n m = m : replicate' (n -1) m

--d)
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) | a == x = True
              | otherwise = elem' a xs

-- Questao 6
recursivaMerge :: Ord a => [a] -> [a] -> [a]
recursivaMerge [] ys = ys
recursivaMerge xs [] = xs
recursivaMerge (x:xs) (y:ys) | x <= y    = x : recursivaMerge xs (y:ys)
                             | otherwise = y : recursivaMerge (x:xs) ys

-- Questao 7
metades :: [a] -> ([a], [a])
metades xs = splitAt (length xs `div` 2) xs

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = recursivaMerge (mergesort ys) (mergesort zs)
                where
                    (ys, zs) = metades xs

-- Questao 8
--a)
somaLista :: Num a => [a] -> a
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

--b)
numElem :: [a] -> Int
numElem [_] = 0
numElem (_:xs) = 1 + numElem xs

--c)
ultimo :: [a] -> a
ultimo [x] = x
ultimo (_:xs) = ultimo xs