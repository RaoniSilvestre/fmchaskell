module Nat where

import Prelude hiding (sum, mult, exp, quot, min)

data Nat = O | S Nat
    deriving ( Eq , Show )

double :: Nat -> Nat 
double O = O
double (S n) =  (S (S (double n)))

sum :: Nat -> Nat -> Nat
sum n O = n
sum n (S m) = S(sum n m)   

mult :: Nat -> Nat -> Nat
mult n O = O
mult n (S m) = sum (mult n m) n

exp :: Nat -> Nat -> Nat
exp n O = S O
exp n (S m) = mult (exp n m) n