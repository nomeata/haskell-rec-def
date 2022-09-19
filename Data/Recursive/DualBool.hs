{-# LANGUAGE TypeApplications #-}

{- | The type @RDualBool@ is like 'Bool', but allows recursive definitions:

>>> :{
  let x = RDB.true
      y = x RDB.&& z
      z = y RDB.|| RDB.false
  in RDB.get x
:}
True


This finds the greatest solution, i.e. prefers 'True' over 'False':

>>> :{
  let x = x RDB.&& y
      y = y RDB.&& x
  in (RDB.get x, RDB.get y)
:}
(True,True)

Use @RBool@ from "Data.Recursive.Bool" if you want the least solution.

-}
module Data.Recursive.DualBool (RDualBool, module Data.Recursive.DualBool) where

import Data.Coerce
import Data.Monoid

import Data.Recursive.Internal
import qualified Data.Propagator.Purify as Purify
import Data.Propagator.P2

-- $setup
-- >>> :load Data.Recursive.Bool Data.Recursive.DualBool
-- >>> :module - Data.Recursive.Bool Data.Recursive.DualBool
--
-- >>> :set -XFlexibleInstances
-- >>> import Test.QuickCheck
-- >>> import qualified Data.Recursive.Bool as RB
-- >>> instance Arbitrary RB.RBool where arbitrary = RB.mk <$> arbitrary
-- >>> instance Show RB.RBool where show = show . RB.get
--
-- >>> import qualified Data.Recursive.DualBool as RDB
-- >>> instance Arbitrary RDB.RDualBool where arbitrary = RDB.mk <$> arbitrary
-- >>> instance Show RDB.RDualBool where show = show . RDB.get

-- | Extracts the value of a 'RDualBool'
get :: RDualBool -> Bool
get (RDualBool p) = Prelude.not (Purify.get p)

-- | prop> RDB.get (RDB.mk b) === b
mk :: Bool -> RDualBool
mk b = RDualBool $ Purify.mk (Prelude.not b)

-- | prop> RDB.get RDB.true == True
true :: RDualBool
true = RDualBool $ Purify.mk False

-- | prop> RDB.get RDB.false == False
false :: RDualBool
false = RDualBool $ Purify.mk True

-- | prop> RDB.get (r1 RDB.&& r2) === (RDB.get r1 && RDB.get r2)
(&&) :: RDualBool -> RDualBool -> RDualBool
(&&) = coerce $ Purify.def2 $ \p1 p2 p -> do
    whenTop p1 (setTop p)
    whenTop p2 (setTop p)

-- | prop> RDB.get (r1 RDB.|| r2) === (RDB.get r1 || RDB.get r2)
(||) :: RDualBool -> RDualBool -> RDualBool
(||) = coerce $ Purify.def2 $ \p1 p2 p ->
    whenTop p1 (whenTop p2 (setTop p))

-- | prop> RDB.get (RDB.and rs) === and (map RDB.get rs)
and :: [RDualBool] -> RDualBool
and = coerce $ Purify.defList $ \ps p ->
    mapM_ @[] (`implies` p) ps

-- | prop> RDB.get (RDB.or rs) === or (map RDB.get rs)
or :: [RDualBool] -> RDualBool
or = coerce $ Purify.defList go
  where
    go [] p = setTop p
    go (p':ps) p = whenTop p' (go ps p)

-- | prop> RDB.get (RDB.not r1) === not (RB.get r1)
not :: RBool -> RDualBool
not = coerce $ Purify.def1 $ \p1 p -> do
    implies p1 p
