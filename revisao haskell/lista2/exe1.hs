{--
Usando compreensão de listas, forneça uma expressão que calcula a 
soma 1² +2²+...+100² dos quadrados dos primeiros 100 números inteiros.
--}
soma = sum [x^2 | x <- [0..100]]