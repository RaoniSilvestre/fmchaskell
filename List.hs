{-# LANGUAGE GADTs #-}

module List where
import Prelude hiding (
    replicate, filter, all, any, zip, pwAdd, map,
    sum,mult, exp, min, max, isEven, 
    minimum, maximum,(<),(<=))
import Nat
replicate :: Nat -> a -> [a]
replicate O _ = []
replicate (S n) a = a : replicate n a

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

