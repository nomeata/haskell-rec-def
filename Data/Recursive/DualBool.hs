{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Data.Recursive.DualBool
  ( R
  , getRDual
  , module Data.Recursive.DualBool
  ) where

import Data.Coerce
import Data.Monoid

import Data.Recursive.R.Internal
import Data.Recursive.R
import Data.Recursive.Propagator.Bool
import Data.Recursive.Propagator.Class

rTrue :: R (Dual Bool)
rTrue = r (Dual True)

rFalse :: R (Dual Bool)
rFalse = r (Dual False)

(|||) :: R (Dual Bool) -> R (Dual Bool) -> R (Dual Bool)
(|||) = defR2 $ coerce $ \p1 p2 p ->
    whenTrue p1 (whenTrue p2 (setTrue p))

(&&&) :: R (Dual Bool) -> R (Dual Bool) -> R (Dual Bool)
(&&&) = defR2 $ coerce $ \p1 p2 p -> do
    whenTrue p1 (setTrue p)
    whenTrue p2 (setTrue p)

ror :: [R (Dual Bool)] -> R (Dual Bool)
ror = defRList $ coerce go
  where
    go [] p = setTrue p
    go (p':ps) p = whenTrue p' (go ps p)

rand :: [R (Dual Bool)] -> R (Dual Bool)
rand = defRList $ coerce $ \ps p ->
    mapM_ @[] (`implies` p) ps

rnot :: R Bool -> R (Dual Bool)
rnot = defR1 $ coerce $ \p1 p -> do
    whenTrue p1 (setTrue p)
