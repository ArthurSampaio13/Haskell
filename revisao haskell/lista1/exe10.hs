{--
Implemente a função mod2, que retorna o resto de uma divisão de inteiros. OBS: não é permitido usar a função mod nem a função rem da biblioteca.
--}
meuMod :: Int -> Int -> Int
meuMod a b = a - (a `div` b * b )