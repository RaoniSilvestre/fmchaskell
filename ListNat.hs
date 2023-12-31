{-# LANGUAGE GADTs #-}

module ListNat where

import Prelude hiding (
    length, concat,sumList,sum,productList,mult,append,reverse,
    allEven,allOdd,allZero,anyEven,anyOdd,anyZero,addNat, exp, min, max,
    minimum, maximum, enumFromTo, (<),(<=), take, drop, init, last)
import Nat

data ListNat = Empty | Cons Nat ListNat
    deriving( Eq, Show)

length :: ListNat -> Nat
length Empty = O
length (Cons x xs) = S(length xs)

concat :: ListNat -> ListNat -> ListNat
concat Empty xs = xs
concat (Cons x xs) ys = Cons x (concat xs ys)

sumList :: ListNat -> Nat
sumList Empty = O
sumList (Cons x xs) = sum x (sumList xs)

productList :: ListNat -> Nat
productList Empty = (S O)
productList (Cons x xs) = mult x (productList xs)

append :: Nat -> ListNat -> ListNat
append x Empty = (Cons x Empty)
append x (Cons y ys) = Cons y (append x ys)

reverse :: ListNat -> ListNat
reverse Empty = Empty
reverse (Cons x xs) = append x (reverse xs)

anyEven :: ListNat -> Bool
anyEven Empty = False
anyEven (Cons x xs)
  | isEven x = True 
  | otherwise = anyEven xs
 
anyOdd :: ListNat -> Bool
anyOdd Empty = False
anyOdd (Cons x xs)
  | isOdd x = True
  | otherwise = anyOdd xs

anyZero :: ListNat -> Bool
anyZero Empty = False
anyZero (Cons x xs)
  | isZero x   = True
  | otherwise  = anyZero xs

allEven :: ListNat -> Bool
allEven Empty = True
allEven (Cons x xs) 
  | isEven x  = allEven xs
  | otherwise = False

allOdd :: ListNat -> Bool
allOdd Empty = True
allOdd (Cons x xs)
  | isOdd x   = allOdd xs
  | otherwise = False

allZero :: ListNat -> Bool
allZero Empty = True
allZero (Cons x xs)
  | isZero x   = allZero xs
  | otherwise  = False

addNat :: Nat -> ListNat -> ListNat
addNat _ Empty = Empty
addNat n (Cons x xs) = Cons (sum x n) (addNat n xs)

multNat :: Nat -> ListNat -> ListNat
multNat _ Empty = Empty
multNat n (Cons x xs) = Cons (mult x n) (multNat n xs)

expNat :: Nat -> ListNat -> ListNat
expNat _ Empty       = Empty
expNat n (Cons x xs) = Cons (exp x n) (expNat n xs)

minimum :: ListNat -> Nat
minimum (Cons x Empty) = x
minimum (Cons x xs)    = min x (minimum xs)  

maximum :: ListNat -> Nat
maximum (Cons x Empty) = x
maximum (Cons x xs)    = max x (maximum xs)

enumFromTo :: Nat -> Nat -> ListNat 
enumFromTo n m
  | n <= m    = Cons n (enumFromTo (S n) m)
  | otherwise = Empty 

enumTo :: Nat -> ListNat
enumTo n = enumFromTo O n 

take :: Nat-> ListNat -> ListNat
take O _ = Empty
take (S n) (Cons x xs) = Cons (x) (take n xs) 

drop :: Nat -> ListNat -> ListNat
drop O xs = xs
drop (S n) (Cons x xs) = drop n xs

head :: ListNat -> Nat
head (Cons x xs) = x

tail :: ListNat -> ListNat
tail (Cons x xs) = xs

init :: ListNat -> ListNat
init (Cons x Empty) = Empty
init (Cons x xs) = Cons x (init xs)

last :: ListNat -> Nat
last (Cons x Empty) = x
last (Cons _ xs) = last xs

pwAdd :: ListNat -> ListNat -> ListNat
pwAdd (Cons x xs) (Cons y ys) = (Cons (sum x y) (pwAdd xs ys))
pwAdd _ _ = Empty

pwMult :: ListNat -> ListNat -> ListNat
pwMult (Cons x xs) (Cons y ys) = (Cons (mult x y) (pwMult xs ys))
pwMult _ _ = Empty

filterEven :: ListNat -> ListNat
filterEven (Cons x xs)
  | (isEven x) = (Cons x (filterEven xs)) 
  | otherwise  = (filterEven xs) 
filterEven _ = Empty

filterOdd :: ListNat -> ListNat
filterOdd (Cons x xs)
  | isOdd x = Cons x (filterOdd xs)
  | otherwise = filterOdd xs
filterOdd _ = Empty

isSorted :: ListNat -> Bool
isSorted (Cons x (Cons y ys))
  | x <= y = isSorted (Cons y ys)
  | otherwise = False
isSorted (Cons x Empty) = True

mix :: ListNat -> ListNat -> ListNat
mix (Cons x xs) (Cons y ys) = Cons x (Cons y (mix xs ys))
mix _ _ = Empty

