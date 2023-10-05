module ListNat where
import Prelude hiding (
    length, concat,sumList,sum,productList,mult,append,reverse,
    allEven,allOdd,allZero,anyEven,anyOdd,anyZero,addNat, exp, min, max,
    minimum, maximum,
    Bool, True, False)
import Nat
import Bool
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

productList :: ListNat -> Nat
productList Empty = (S O)
productList (Cons x xs) = mult x (productList xs)

append :: Nat -> ListNat -> ListNat
append x Empty = (Cons x Empty)
append x (Cons y ys) = Cons y (append x ys)

reverse :: ListNat -> ListNat
reverse Empty = Empty
reverse (Cons x xs) = append x (reverse xs)

anyEven :: ListNat -> Bool
anyEven Empty = False
anyEven (Cons x xs) = if_then_else (isEven x) True (anyEven xs)
 
anyOdd :: ListNat -> Bool
anyOdd Empty = False
anyOdd (Cons x xs) = if_then_else (isOdd x) True (anyOdd xs)

anyZero :: ListNat -> Bool
anyZero Empty = False
anyZero (Cons x xs) = if_then_else (isZero x) True (anyZero xs)

allEven :: ListNat -> Bool
allEven Empty = True
allEven (Cons x xs) = if_then_else (isEven x) (allEven xs) False

allOdd :: ListNat -> Bool
allOdd Empty = True
allOdd (Cons x xs) = if_then_else (isOdd x) (allOdd xs) False

allZero :: ListNat -> Bool
allZero Empty = True
allZero (Cons x xs) = if_then_else (isZero x) (allZero xs) False

addNat :: Nat -> ListNat -> ListNat
addNat _ Empty = Empty
addNat n (Cons x xs) = Cons (sum x n) (addNat n xs)

multNat :: Nat -> ListNat -> ListNat
multNat _ Empty = Empty
multNat n (Cons x xs) = Cons (mult x n) (multNat n xs)

expNat :: Nat -> ListNat -> ListNat
expNat _ Empty = Empty
expNat n (Cons x xs) = Cons (exp x n) (expNat n xs)

minimum :: ListNat -> Nat
minimum (Cons x Empty) = x
minimum (Cons x xs) = min x (minimum xs)  

maximum :: ListNat -> Nat
maximum (Cons x Empty) = x
maximum (Cons x xs) = max x (maximum xs)