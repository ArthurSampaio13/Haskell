sequencia :: Int -> Float
sequencia 1 = sqrt 6
sequencia n = sqrt (6 + sequencia (n - 1))