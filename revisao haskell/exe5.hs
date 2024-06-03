{--
Implemente em Haskell a função do ou-exclusivo
--}
ouExclusivo :: Bool -> Bool -> Bool
ouExclusivo a b = (a || b) && not (a && b)