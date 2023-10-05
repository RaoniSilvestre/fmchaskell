module ListNat where
import Prelude hiding (length, concat,sumList,sum,productList,mult,append,reverse)
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


