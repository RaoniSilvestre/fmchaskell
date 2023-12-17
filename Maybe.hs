{-# LANGUAGE GADTs #-}

module Maybe where

import Prelude hiding (Maybe(..))

data Maybe a where
    Nothing :: Maybe a
    Just :: a -> Maybe a
    deriving(Eq,Show)