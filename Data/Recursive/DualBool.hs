module Data.Recursive.DualBool
  ( R
  , getRDual
  , module Data.Recursive.DualBool
  ) where

import Data.Coerce
import Data.Monoid

import Data.Recursive.R.Internal
import Data.Recursive.R
import Data.Recursive.Propagator.Naive

rTrue :: R (Dual Bool)
rTrue = r (Dual True)

rFalse :: R (Dual Bool)
rFalse = r (Dual False)

(&&&) :: R (Dual Bool) -> R (Dual Bool) -> R (Dual Bool)
(&&&) = defR2 $ lift2 $ coerce (&&)

(|||) :: R (Dual Bool) -> R (Dual Bool) -> R (Dual Bool)
(|||) = defR2 $ lift2 $ coerce (||)

rand :: [R (Dual Bool)] -> R (Dual Bool)
rand = defRList $ liftList $ coerce (and :: [Bool] -> Bool)

ror :: [R (Dual Bool)] -> R (Dual Bool)
ror = defRList $ liftList $ coerce (or :: [Bool] -> Bool)

rnot :: R Bool -> R (Dual Bool)
rnot = defR1 $ lift1 $ coerce not
