{-# LANGUAGE GADTs #-}

module List where
import Prelude hiding (
    replicate,
    sum,mult, exp, min, max,
    minimum, maximum,(<),(<=),
    Bool, True, False)
import Nat
import Bool
data List a = Empty | Cons a (List a)
    deriving( Eq, Show)

replicate :: Nat -> a -> List a
replicate O _ = Empty
replicate (S n) a = Cons a (replicate n a)

