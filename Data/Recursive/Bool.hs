{-# LANGUAGE TypeApplications #-}

{- | The type @R Bool@ is like 'Bool', but allows recursive definitions:

>>> :{
  let x = rTrue
      y = x &&& z
      z = y ||| rFalse
  in getR x
:}
True


This finds the least solution, i.e. prefers 'False' over 'True':

>>> :{
  let x = x &&& y
      y = y &&& x
  in (getR x, getR y)
:}
(False,False)

Use @R (Dual Bool)@ from "Data.Recursive.DualBool" if you want the greatest solution.

-}
module Data.Recursive.Bool
  ( R
  , getR
  , module Data.Recursive.Bool
  ) where


import Data.Coerce
import Data.Monoid

import Data.Recursive.R.Internal
import Data.Recursive.R
import Data.Recursive.Propagator.P2

-- $setup
-- >>> :set -XFlexibleInstances
-- >>> import Test.QuickCheck
-- >>> instance Arbitrary (R Bool) where arbitrary = mkR <$> arbitrary
-- >>> instance Show (R Bool) where show = show . getR
-- >>> instance Arbitrary (R (Dual Bool)) where arbitrary = mkR <$> arbitrary
-- >>> instance Show (R (Dual Bool)) where show = show . getR

-- | prop> getR rTrue == True
rTrue :: R Bool
rTrue = mkR True

-- | prop> getR rFalse == False
rFalse :: R Bool
rFalse = mkR False

{- Using the naive propagator:

(&&&) :: R Bool -> R Bool -> R Bool
(&&&) = defR2 $ lift2 (&&)

(|||) :: R Bool -> R Bool -> R Bool
(|||) = defR2 $ lift2 (||)

rand :: [R Bool] -> R Bool
rand = defRList $ liftList and

ror :: [R Bool] -> R Bool
ror = defRList $ liftList or

rnot :: R (Dual Bool) -> R Bool
rnot = defR1 $ lift1 $ coerce not

-}

-- | prop> getR (r1 &&& r2) === (getR r1 && getR r2)
(&&&) :: R Bool -> R Bool -> R Bool
(&&&) = defR2 $ coerce $ \p1 p2 p ->
    whenTop p1 (whenTop p2 (setTop p))

-- | prop> getR (r1 ||| r2) === (getR r1 || getR r2)
(|||) :: R Bool -> R Bool -> R Bool
(|||) = defR2 $ coerce $ \p1 p2 p -> do
    whenTop p1 (setTop p)
    whenTop p2 (setTop p)

-- | prop> getR (rand rs) === and (map getR rs)
rand :: [R Bool] -> R Bool
rand = defRList $ coerce go
  where
    go [] p = setTop p
    go (p':ps) p = whenTop p' (go ps p)

-- | prop> getR (ror rs) === or (map getR rs)
ror :: [R Bool] -> R Bool
ror = defRList $ coerce $ \ps p ->
    mapM_ @[] (`implies` p) ps

-- | prop> getR (rnot r1) === not (getRDual r1)
rnot :: R (Dual Bool) -> R Bool
rnot = defR1 $ coerce $ \p1 p -> do
    implies p1 p
