{-# LANGUAGE GADTs #-}
module GenTree where

import Nat

import Prelude hiding (
    replicate, filter, all, any, zip, pwAdd, reverse, iter,
    sum,mult, exp, min, max, isEven, 
    minimum, maximum,(<),(<=), concat)


data GenTree a 
  Node : a -> [GenTree a] -> GenTree a

tips :: GenTree a -> Nat
tips (Node x []) = S O
tips (Node x (y:ys)) = sum (tips y) (tips (Node x ys))

forks :: GenTree a -> Nat
forks (Node x []) = O
forks (Node x (y:ys)) = S (sum (forks y) (forks (Node x ys)))

depth :: GenTree a -> Nat
depth (Node x []) = O
depth (Node x (y:ys)) = S (max (depth y) (depth (Node x ys)))

