module Nat where

import Prelude hiding (monus,sum, mult, exp, pred, fact, fib, quot, min, max, div, quot, rem, gcd, lcm)

data Nat = O | S Nat
    deriving ( Eq , Show )



double :: Nat -> Nat 
double O = O
double (S n) =  (S (S (double n)))

sum :: Nat -> Nat -> Nat
sum n O = n
sum n (S m) = S(sum n m)   

monus :: Nat -> Nat -> Nat
monus O _ = O
monus n O = n
monus (S n) (S m) = monus n m

mult :: Nat -> Nat -> Nat
mult n O = O
mult n (S m) = sum (mult n m) n

exp :: Nat -> Nat -> Nat
exp n O = S O
exp n (S m) = mult (exp n m) n

min :: Nat -> Nat -> Nat
min O O = O
min O n = O
min n O = O
min (S n) (S m) = S(min n m)

max :: Nat -> Nat -> Nat
max O O = O
max O n = n
max n O = n
max (S n) (S m) = S(max n m)

pred :: Nat -> Nat
pred O = O
pred (S n) = n


