{-# LANGUAGE TypeFamilies #-}
module Data.Recursive.MustBe.Internal where

import Data.Recursive.Internal
import Data.Recursive.Class

data MustBe

instance Order MustBe where
    type Val MustBe = Bool
    bottom = False
