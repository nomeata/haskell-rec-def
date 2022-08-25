{-# LANGUAGE TypeFamilies #-}
module Data.Recursive.CanBe.Internal where

import Data.Recursive.Internal
import Data.Recursive.Class

data CanBe

instance Order CanBe where
    type Val CanBe = Bool
    bottom = True
