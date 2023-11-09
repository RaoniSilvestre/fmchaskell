module Int where
import Nat
import Prelude hiding(simp,soma,Int, sum)

data Int = MkInt Nat Nat
    deriving ( Eq , Show )

sumInt :: Int -> Int -> Int
sumInt (MkInt n m) (MkInt u v) = canon(MkInt (sum n u) (sum m v)) 

canon :: Int -> Int
canon (MkInt n O) = MkInt n O
canon (MkInt O m) = MkInt O m
canon (MkInt (S n) (S m)) = canon (MkInt n m)
