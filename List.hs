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
