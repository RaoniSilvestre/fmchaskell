{-# LANGUAGE GADTs #-}

module List where
import Prelude hiding (Bool, True, False)
import Nat
import Bool
data List a = Empty | Cons a (List a)
    deriving( Eq, Show)

