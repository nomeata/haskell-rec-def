{-# LANGUAGE TypeFamilies #-}
module Data.Recursive.MustBe.Internal where

import Data.Recursive.R.Internal

data MustBe

instance Order MustBe where
    type Val MustBe = Bool
    bottom = False
