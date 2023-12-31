{-# LANGUAGE GADTs #-}
module BinTree where

import Nat
import Path

import Prelude hiding (
    replicate, filter, all, any, zip, pwAdd, reverse, iter,
    sum,mult, exp, min, max, isEven, 
    minimum, maximum,(<),(<=), concat, Path, Right, Left)

data BinTree a = Tip a | BinTree (BinTree a) (BinTree a)
    deriving ( Eq , Show)

tips :: BinTree a -> Nat
tips (Tip x) = S O
tips (BinTree l r) = sum (tips l) (tips r)

forks :: BinTree a -> Nat
forks (Tip x) = O
forks (BinTree l r) = S (sum (forks l) (forks r))

depth :: BinTree a -> Nat
depth (Tip x) = O
depth (BinTree l r) = S (max (depth l) (depth r))

leaves :: BinTree a -> [a]
leaves (Tip x) = [x]
leaves (BinTree l r) = leaves l ++ leaves r


eficleaves :: BinTree a -> [a] -> [a]
eficleaves (Tip x) xs = x:xs
eficleaves (BinTree l r) xs = eficleaves l (eficleaves r xs)

mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Tip x) = Tip (f x)
mapTree f (BinTree l r) = BinTree (mapTree f l) (mapTree f r)

mirror :: BinTree a -> BinTree a
mirror (BinTree l r) = BinTree  (mirror r) (mirror l)
mirror t = t

sumTree :: BinTree Nat -> Nat
sumTree (Tip x) = x
sumTree (BinTree l r) = sum (sumTree l) (sumTree r) 

findTree :: (Eq a) => a -> BinTree a -> [[Path]]
findTree x (Tip y) = if x == y then [[]] else []
findTree x (BinTree l r) = map (Left:) (findTree x l) ++ (map (Right:) (findTree x r))

foldTree :: (a -> a -> a) -> a -> BinTree a -> a
foldTree f z (Tip x) = f x z
foldTree f z (BinTree l r) = foldTree f (foldTree f z r) l