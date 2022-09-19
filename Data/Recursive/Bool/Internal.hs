{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE TypeApplications #-}

{- |
 The internals of the "Data.Recursive.Bool" and "Data.Recursive.DualBool"
 module. Only import this if you want to extend the APIs for these functions.
-}
module Data.Recursive.Bool.Internal where

import qualified Data.Propagator.Purify as Purify
import Data.Propagator.P2

-- | Like 'Bool', but admits recursive definitions, preferring the least solution.
newtype RBool = RBool (Purify.Purify P2)

-- | Like 'Bool', but admits recursive definitions, preferring the greatest solution.
newtype RDualBool = RDualBool (Purify.Purify P2)
