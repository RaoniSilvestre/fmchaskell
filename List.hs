{-# LANGUAGE GADTs #-}

module List where
import Prelude hiding (
    replicate, filter, all, any, zip, pwAdd, map, reverse, iter,
    sum,mult, exp, min, max, isEven, 
    minimum, maximum,(<),(<=), concat)
    
import Nat

replicate :: Nat -> a -> [a]
replicate O _ = []
replicate (S n) a = a : replicate n a

concat :: [a] -> [a] -> [a]
concat [] xs = xs
concat (x : xs) ys = x : (concat xs ys)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x : xs)
  | f x = x : filter f xs
  | otherwise = filter f xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all f (x : xs)
  | f x = all f xs
  | otherwise = False

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (x : xs)
  | f x = True
  | otherwise = any f xs


pw :: (a -> b -> c) -> [a] -> [b] -> [c]
pw f (x : xs) (y : ys) = (f x y) : (pw f xs ys)
pw f _ _ = []


zip = pw (,)
pwAdd = pw (sum)
pwMult = pw (mult)


takewhile :: (a -> Bool) -> [a] -> [a]
takewhile _ [] = []
takewhile f (x : xs)
  | f x = x : takewhile f xs
  | otherwise = []


dropwhile :: (a -> Bool) -> [a] -> [a]
dropwhile _ [] = []
dropwhile f (x : xs)
  | f x = dropwhile f xs
  | otherwise = x : xs


map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

append :: a -> [a] -> [a]
append x [] = [x]
append x (y:ys) = y : (append x ys)

reverse :: [a] -> [a]
reverse [] = []
reverse (n:ns) = append n (reverse ns)

iter :: (a -> a) -> Nat -> a -> a
iter _ O a     = a
iter f (S n) a = f (iter f n a)

fold :: (a -> a -> a) -> a -> [a] -> a
fold f x []     = x
fold f x (y:ys) =  f y (fold f x ys)