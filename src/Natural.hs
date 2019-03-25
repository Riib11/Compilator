module Natural where

data Nat = Zero | Succ Nat

instance Show Nat where show = show . nat_to_int

nat_to_int :: Nat -> Int
nat_to_int n = case n of { Zero -> 0 ; Succ n' -> 1 + nat_to_int n' }

int_to_nat :: Int -> Nat
int_to_nat x | x <= 0 = Zero | otherwise = Succ $ int_to_nat (x - 1)

