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
multi (Suc x) y = somar (multi x y) y

-- Questao 2
data ArvoreBinaria a = Folha1 a | No1 (ArvoreBinaria a) a (ArvoreBinaria a) deriving Show

existe :: Ord a => a -> ArvoreBinaria a -> Bool 
existe x (Folha1 y) = x == y
existe x (No1 esq meio dir) = x == meio || existe x esq || existe x dir

-- Questao 3
data Arvore a = Folha a | No (Arvore a) (Arvore a)

contaFolhas :: Arvore a -> Int
contaFolhas (Folha x) = 1
contaFolhas (No esq dir) = contaFolhas esq + contaFolhas dir

balanceada :: Arvore a -> Bool
balanceada (Folha x) = True
balanceada (No esq dir) = contaFolhas esq == contaFolhas dir

-- Questao 4
divideLista :: [a] -> ([a], [a])
divideLista lista = splitAt (length lista `div` 2) lista

balancear :: [a] -> Arvore a
balancear [a] = Folha a
balancear lista = No (balancear esq) (balancear dir)
                    where
                        (esq, dir) = divideLista lista

-- Questao 5
data Expr = Val Int | Add Expr Expr

avaliar :: Expr -> Int
avaliar (Val a) = a
avaliar (Add x y) = avaliar x + avaliar y

-- Questao 6
data Expr2 = Val2 Int | Op2 Expr2 Expr2

folde :: (Int -> a) -> (a -> a -> a) -> Expr2 -> a
folde f _ (Val2 i) = f i
folde f g (Op2 e1 e2) = g (folde f g e1) (folde f g e2)

-- Questao 7
eval :: Expr2 -> Int
eval = folde id (+)