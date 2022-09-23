{-# LANGUAGE TypeApplications #-}

{- | The type @RBool@ is like 'Bool', but allows recursive definitions:

>>> :{
  let x = RB.true
      y = x RB.&& z
      z = y RB.|| RB.false
  in RB.get x
:}
True


This finds the least solution, i.e. prefers 'False' over 'True':

>>> :{
  let x = x RB.&& y
      y = y RB.&& x
  in (RB.get x, RB.get y)
:}
(False,False)

Use 'Data.Recursive.DualBool.RDualBool' from "Data.Recursive.DualBool" if you want the greatest solution.

-}
module Data.Recursive.Bool (RBool, module Data.Recursive.Bool) where


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

-- | Extracts the value of a 'RBool'
get :: RBool -> Bool
get (RBool p) = Purify.get p

-- | prop> RB.get (RB.mk b) === b
mk :: Bool -> RBool
mk b = openR $ Purify.mk b

-- | prop> RB.get RB.true == True
true :: RBool
true = openR $ Purify.mk True

-- | prop> RB.get RB.false == False
false :: RBool
false = openR $ Purify.mk False

-- | prop> RB.get (r1 RB.&& r2) === (RB.get r1 && RB.get r2)
(&&) :: RBool -> RBool -> RBool
(&&) = openR $ Purify.def2 $ \p1 p2 p ->
    whenTop p1 (whenTop p2 (setTop p))

-- | prop> RB.get (r1 RB.|| r2) === (RB.get r1 || RB.get r2)
(||) :: RBool -> RBool -> RBool
(||) = openR $ Purify.def2 $ \p1 p2 p -> do
    whenTop p1 (setTop p)
    whenTop p2 (setTop p)

-- | prop> RB.get (RB.and rs) === and (map RB.get rs)
and :: [RBool] -> RBool
and = openR $ Purify.defList $ go
  where
    go [] p = setTop p
    go (p':ps) p = whenTop p' (go ps p)

-- | prop> RB.get (RB.or rs) === or (map RB.get rs)
or :: [RBool] -> RBool
or = openR $ Purify.defList $  \ps p ->
    mapM_ @[] (`implies` p) ps

-- | prop> RB.get (RB.not r1) === not (RDB.get r1)
not :: RDualBool -> RBool
not = openR $ Purify.def1 $ \p1 p -> do
    implies p1 p

-- | The identity function. This is useful when tying the knot, to avoid a loop that bottoms out:
--
-- > let x = x in RB.get x
--
-- will not work, but
--
-- >>> let x = RB.id x in RB.get x
-- False
--
-- does.
--
-- prop> RB.get (RB.id r) === RB.get r
id :: RBool -> RBool
id = openR $ Purify.def1 $ \p1 p ->
    implies p1 p
