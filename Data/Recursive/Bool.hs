module Data.Recursive.Bool
  ( R
  , getR
  , module Data.Recursive.Bool
  ) where

import Data.Coerce
import Data.Monoid

import Data.Recursive.R.Internal
import Data.Recursive.R
import Data.Recursive.Propagator.Bool

rTrue :: R Bool
rTrue = r True

rFalse :: R Bool
rFalse = r False

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

(&&&) :: R Bool -> R Bool -> R Bool
(&&&) = defR2 $ \p1 p2 p ->
    whenTrue p1 (whenTrue p2 (setTrue p))

(|||) :: R Bool -> R Bool -> R Bool
(|||) = defR2 $ \p1 p2 p -> do
    whenTrue p1 (setTrue p)
    whenTrue p2 (setTrue p)

rand :: [R Bool] -> R Bool
rand = defRList go
  where
    go [] p = setTrue p
    go (p':ps) p = whenTrue p' (go ps p)

ror :: [R Bool] -> R Bool
ror = defRList $ \ps p ->
    mapM_ (`implies` p) ps
