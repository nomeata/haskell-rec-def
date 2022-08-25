{-# LANGUAGE TypeFamilies #-}
module Data.Recursive.CanBe.Internal where

import Data.Recursive.R.Internal

data CanBe

instance Order CanBe where
    type Val CanBe = Bool
    bottom = True
