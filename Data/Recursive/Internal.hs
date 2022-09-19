{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE TypeApplications #-}

{- |
 This modules contains the newtype definitions backing

  * "Data.Recursive.Bool"
  * "Data.Recursive.DualBool"
  * "Data.Recursive.Set"

  Access to the newtype contructor can break the guarantees of these modules.
  Only import this if you want to extend the APIs for these types.
-}
module Data.Recursive.Internal where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Propagator.Purify as Purify
import Data.Propagator.P2
import Data.Propagator.Naive

-- | Like 'Bool', but admits recursive definitions, preferring the least solution.
newtype RBool = RBool (Purify.Purify P2)

-- | Like 'Bool', but admits recursive definitions, preferring the greatest solution.
newtype RDualBool = RDualBool (Purify.Purify P2)

-- | Like 'S.Set', but admits recursive definitions.
newtype RSet a = RSet (Purify.Purify (Prop (S.Set a)))

-- | Like 'M.Map', but admits recursive definitions.
data RMap a b = RMap (RSet a) (M.Map a b)
