data Nat = Zero | Suc Nat deriving Show

-- Questao 1
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Suc n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Suc (int2nat (n-1))

somar :: Nat -> Nat -> Nat
somar m n = int2nat (nat2int m + nat2int n)

somar2 :: Nat -> Nat -> Nat
somar2 Zero n = n
somar2 (Suc m) n = Suc (somar m n)

multi :: Nat -> Nat -> Nat
multi _ Zero = Zero
multi Zero _ = Zero
multi (Suc m) n = somar (multi m n) n

-- Questao 2
data ArvoreBinaria a = Folha1 a | No1 (ArvoreBinaria a) a (ArvoreBinaria a) deriving Show

existe :: Ord a => a -> ArvoreBinaria a -> Bool
existe v (Folha1 f) = f == v
existe v (No1 esq meio dir) = v == meio || existe v esq || existe v dir

-- Questao 3
data Arvore a = Folha a | No (Arvore a) (Arvore a) deriving Show
contaFolhas :: Arvore a -> Int
contaFolhas (Folha a) = 1
contaFolhas (No esq dir) = contaFolhas esq + contaFolhas dir

balanceada :: Arvore a -> Bool
balanceada (Folha a) = True
balanceada (No esq dir) = contaFolhas esq == contaFolhas dir

-- Questao 4
dividir :: [a] -> ([a], [a])
dividir lista = splitAt (length lista `div` 2) lista

balancear :: [a] -> Arvore a 
balancear [a] = Folha a
balancear lista = No (balancear esq) (balancear dir)
                    where
                        (esq, dir) = dividir lista

-- Questao 5
data Expr = Val Int | Add Expr Expr

avaliar :: Expr -> Int
avaliar (Val a) = a
avaliar (Add x y) = avaliar x + avaliar y

-- Questao 6




