module Int where

import Prelude hiding(simp,soma,Int)

data Int = O | S Int | P Int
    deriving ( Eq , Show )


simp :: Int -> Int
simp (P (S x)) = x
simp (S (P x)) = x
simp x = x

soma :: Int -> Int -> Int
soma x O = x
soma O x = x
soma x (S y) = simp(S(soma x y))
soma x (P y) = simp(P(soma x y))
