module Bool where

import Prelude hiding(True, False, Bool)

data Bool = False | True
    deriving( Eq, Show)

if_then_else :: Bool -> Bool -> Bool -> Bool
if_then_else True n _ = n
if_then_else False _ m = m
