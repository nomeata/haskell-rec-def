module Data.Recursive.Bool where

import Data.Coerce
import Data.Monoid

import Data.Recursive.R.Internal
import Data.Recursive.R

rTrue :: R Bool
rTrue = r True

rFalse :: R Bool
rFalse = r False

(&&&) :: R Bool -> R Bool -> R Bool
(&&&) = liftR2 (&&)

(|||) :: R Bool -> R Bool -> R Bool
(|||) = liftR2 (||)

rand :: [R Bool] -> R Bool
rand = liftRList and

ror :: [R Bool] -> R Bool
ror = liftRList or

rnot :: R (Dual Bool) -> R Bool
rnot = mapR $ coerce not
