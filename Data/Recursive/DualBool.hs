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
import Data.Recursive.Propagator.P2

rTrue :: R (Dual Bool)
rTrue = r (Dual True)

rFalse :: R (Dual Bool)
rFalse = r (Dual False)

(|||) :: R (Dual Bool) -> R (Dual Bool) -> R (Dual Bool)
(|||) = defR2 $ coerce $ \p1 p2 p ->
    whenTop p1 (whenTop p2 (setTop p))

(&&&) :: R (Dual Bool) -> R (Dual Bool) -> R (Dual Bool)
(&&&) = defR2 $ coerce $ \p1 p2 p -> do
    whenTop p1 (setTop p)
    whenTop p2 (setTop p)

ror :: [R (Dual Bool)] -> R (Dual Bool)
ror = defRList $ coerce go
  where
    go [] p = setTop p
    go (p':ps) p = whenTop p' (go ps p)

rand :: [R (Dual Bool)] -> R (Dual Bool)
rand = defRList $ coerce $ \ps p ->
    mapM_ @[] (`implies` p) ps

rnot :: R Bool -> R (Dual Bool)
rnot = defR1 $ coerce $ \p1 p -> do
    implies p1 p
