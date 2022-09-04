{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

{- | The type @R (Dual Bool)@ is like 'Bool', but allows recursive definitions:

>>> :{
  let x = rTrue
      y = x &&& z
      z = y ||| rFalse
  in getRDual x
:}
True


This finds the greatest solution, i.e. prefers 'True' over 'False':

>>> :{
  let x = x &&& y
      y = y &&& x
  in (getRDual x, getRDual y)
:}
(True,True)

Use @R Bool@ from "Data.Recursive.Bool" if you want the least solution.

-}
module Data.Recursive.DualBool
  ( R
  , getRDual
  , module Data.Recursive.DualBool
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

-- | prop> getRDual rTrue == True
rTrue :: R (Dual Bool)
rTrue = mkR (Dual True)

-- | prop> getRDual rFalse == False
rFalse :: R (Dual Bool)
rFalse = mkR (Dual False)

-- | prop> getRDual (r1 ||| r2) === (getRDual r1 || getRDual r2)
(|||) :: R (Dual Bool) -> R (Dual Bool) -> R (Dual Bool)
(|||) = defR2 $ coerce $ \p1 p2 p ->
    whenTop p1 (whenTop p2 (setTop p))

-- | prop> getRDual (r1 &&& r2) === (getRDual r1 && getRDual r2)
(&&&) :: R (Dual Bool) -> R (Dual Bool) -> R (Dual Bool)
(&&&) = defR2 $ coerce $ \p1 p2 p -> do
    whenTop p1 (setTop p)
    whenTop p2 (setTop p)

-- | prop> getRDual (ror rs) === or (map getRDual rs)
ror :: [R (Dual Bool)] -> R (Dual Bool)
ror = defRList $ coerce go
  where
    go [] p = setTop p
    go (p':ps) p = whenTop p' (go ps p)

-- | prop> getRDual (rand rs) === and (map getRDual rs)
rand :: [R (Dual Bool)] -> R (Dual Bool)
rand = defRList $ coerce $ \ps p ->
    mapM_ @[] (`implies` p) ps

-- | prop> getRDual (rnot r1) === not (getR r1)
rnot :: R Bool -> R (Dual Bool)
rnot = defR1 $ coerce $ \p1 p -> do
    implies p1 p
