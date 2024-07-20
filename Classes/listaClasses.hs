data Nat = Zero | Suc Nat deriving Show

-- Questa 1
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Suc n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Suc (int2nat (n-1))

somar :: Nat -> Nat -> Nat
somar m n = int2nat (nat2int m + nat2int n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult m (Suc n) = somar m (mult m n)

-- Questao 2
data Arvore a = Folha a | No (Arvore a) a (Arvore a)

existe :: Ord a => a -> Arvore a -> Bool
existe x (Folha a) = x == a
existe x (No esq meio dir) = (x == meio) || existe x esq || existe x dir

-- Questao 3
conta :: Arvore a -> Int
conta (Folha a) = 1
conta (No esq meio dir) = 0 + conta esq + conta dir

balanceada :: Arvore a -> Bool
balanceada (Folha a) = False
balanceada (No esq meio dir) | conta esq == conta dir = True
                             | otherwise = False

-- Questao 4

