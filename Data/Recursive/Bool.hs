module Data.Recursive.Bool
  ( R
  , getR
  , module Data.Recursive.Bool
  ) where

import Data.Coerce
import Data.Monoid

import Data.Recursive.R.Internal
import Data.Recursive.R
import Data.Recursive.Propagator.Naive

rTrue :: R Bool
rTrue = r True

rFalse :: R Bool
rFalse = r False

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
