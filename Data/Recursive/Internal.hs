{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

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

import Data.Coerce

-- Not yet
-- import GHC.TypeError

-- | Like 'Bool', but admits recursive definitions, preferring the least solution.
newtype RBool = RBool (Purify.Purify P2)

-- | Like 'Bool', but admits recursive definitions, preferring the greatest solution.
newtype RDualBool = RDualBool (Purify.Purify P2)

-- | Like 'S.Set', but admits recursive definitions.
newtype RSet a = RSet (Purify.Purify (Prop (S.Set a)))

-- | Like 'M.Map', but admits recursive definitions.
data RMap a b = RMap (RSet a) (M.Map a b)

-- | See 'openR'. Can be extended as needed
type family OpenR a where
    -- newtypes
    OpenR RBool = Purify.Purify P2
    OpenR RDualBool = Purify.Purify P2
    OpenR (RSet a) = Purify.Purify (Prop (S.Set a))
    -- other type constructors (extended as needed)
    OpenR (a -> b) = OpenR a -> OpenR b
    OpenR [a] = [OpenR a]
    -- base type
    OpenR other = other

-- | A constrainted form of 'coerce' that only resolves the newtypes from this
-- module, in a directed way.
--
-- This improves type inference (e.g. if 'Data.Recursive.Set.id' was using
-- 'coerce' instead, it would need a type signature).
--
-- The idiom is [due to Daniel Díaz Carrete](https://discourse.haskell.org/t/haskell-mini-idiom-constraining-coerce/3814?u=nomeata).
openR :: Coercible a (OpenR a) => OpenR a -> a
openR = coerce
