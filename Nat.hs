module Nat where

import Prelude hiding (
    monus,sum, mult, exp, pred, fact,
    fib, quot, min, max, div, quot, 
    rem, gcd, lcm,
    isOdd, isEven, (<=), fib)


data Nat = O | S Nat
    deriving ( Eq , Show )


instance Num Nat where
  (+) = sum
  (*) = mult
  (-) = monus
  negate = error "Negativos não são Nats"
  abs x = x
  signum O = O
  signum _ = S O
  fromInteger = fi

fn :: Nat -> Integer
fn O = 0
fn (S n) = 1 + fn n

fi :: Integer -> Nat
fi n
  | n < 0      = error "Negativos não são Natts"
  | n == 0     = O
  | otherwise  = S (fi(n -1))


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
min O n = O
min n O = O
min (S n) (S m) = S(min n m)

max :: Nat -> Nat -> Nat
max O n = n
max n O = n
max (S n) (S m) = S(max n m)

pred :: Nat -> Nat
pred O = O
pred (S n) = n

eq :: Nat -> Nat -> Bool
eq O O = True
eq (S n) (S m) = eq n m
eq _ _ = False

leq :: Nat -> Nat -> Bool
leq _ O = False
leq O _ = True
leq (S n) (S m) = leq n m

isOdd :: Nat -> Bool
isOdd O = False
isOdd (S O) = True
isOdd (S(S n)) = isOdd n

isEven :: Nat -> Bool
isEven O = True
isEven (S O) = False
isEven (S(S n)) = isEven n

isZero :: Nat -> Bool
isZero O = True
isZero _ = False
 
fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S(S n)) = sum (fib (S n)) (fib n)

(<<) :: Nat -> Nat -> Bool
_ << O = False
O << _ = True
(S x) << (S y) = x << y

(<=) :: Nat -> Nat -> Bool
O <= O = True
_ <= O = False
O <= _ = True
(S x) <= (S y) = x <= y