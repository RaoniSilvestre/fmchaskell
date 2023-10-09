module PairNat where
import Nat

data PairNat = Pair Nat Nat
    deriving ( Eq , Show )

sumPair1 :: PairNat -> PairNat
sumPair1 (Pair x y) = Pair (S x) (S y)