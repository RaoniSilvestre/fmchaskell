module ListNat where
import Prelude hiding (length, concat,sumList,sum,productList,mult,append,reverse)
import Nat

data ListNat = Empty | Cons Nat ListNat
    deriving( Eq, Show)



