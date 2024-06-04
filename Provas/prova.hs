import Data.List (delete, minimum, findIndex)

-- Questao 1
listaTuplas xs = concat [x | (_, x) <- xs]

-- Questao 2    
removeMin xs = delete (minimum xs) xs

-- Questao 3
converter x
    | x >= 359999 = "99:59:59"
    | x >= 3600 = formatar (x `div` 3600) ++ ":" ++ formatar (x `mod` 3600 `div` 60) ++ ":" ++ formatar (x `mod` 60)
    | x >= 60 = "00:" ++ formatar (x `div` 60) ++ ":" ++ formatar (x `mod` 60)
    | otherwise = "00:00:" ++ formatar x
    where
        formatar n
            | n < 10 = '0' : show n
            | otherwise = show n

-- Questao 4
--func :: [(a -> b)] -> [a] -> [(a -> b)]
--func f e = [x | x <- f, f e == False]
-- Supostamente funciona