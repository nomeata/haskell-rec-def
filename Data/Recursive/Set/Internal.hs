{-# LANGUAGE TypeFamilies #-}
{- |
The internals of the "Data.Recursive.Set" module. Only import this if you want
to extend the APIs for these functions.
-}
module Data.Recursive.Set.Internal where

import qualified Data.Set as S
import qualified Data.Propagator.Purify as Purify
import Data.Propagator.Naive

-- | Like 'S.Set' @a@, but admits recursive definitions.
newtype RSet a = RSet (Purify.Purify (Prop (S.Set a)))
