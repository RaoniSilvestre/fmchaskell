{-# LANGUAGE GADTs #-}
module Path where

import Prelude hiding(Path)

data Path = Left | Right
    deriving( Eq, Show)
