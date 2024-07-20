{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant ==" #-}
import Data.List (delete)
dependentes xs = concat [x | (_, x) <- xs]

removeMin xs = delete (minimum xs) xs


converter x
    | x >= 359999 = "99:59:59"
    | x >= 3600 = formatar (x `div` 3600) ++ ":" ++ formatar (x `mod` 3600 `div` 60) ++ ":" ++ formatar (x `mod` 60)
    | x >= 60 = "00:" ++ formatar (x `div` 60) ++ ":" ++ formatar (x `mod` 60)
    | otherwise = "00:00:" ++ formatar x
    where
        formatar n
            | n < 10 = '0' : show n
            | otherwise = show n

--func :: [(a -> b)] -> [a] -> [(a -> b)]
--func func x = [f | f <- func, f x == False]