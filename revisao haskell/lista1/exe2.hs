{--
Fornecidos três valores a, b e c, elaborar uma função que retorne 
quantos desses três valores são maiores que a média entre eles.
--}
media a b c = (if a > media then 1 else 0)+ 
              (if b > media then 1 else 0) +
              (if c > media then 1 else 0)
              where 
                media = (a + b + c) / 3