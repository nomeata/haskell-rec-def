{-# LANGUAGE TypeApplications #-}
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

-- |
-- prop> getR rTrue == True
-- >>> import Test.QuickCheck
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
(&&&) = defR2 $ coerce $ \p1 p2 p ->
    whenTop p1 (whenTop p2 (setTop p))

(|||) :: R Bool -> R Bool -> R Bool
(|||) = defR2 $ coerce $ \p1 p2 p -> do
    whenTop p1 (setTop p)
    whenTop p2 (setTop p)

rand :: [R Bool] -> R Bool
rand = defRList $ coerce go
  where
    go [] p = setTop p
    go (p':ps) p = whenTop p' (go ps p)

ror :: [R Bool] -> R Bool
ror = defRList $ coerce $ \ps p ->
    mapM_ @[] (`implies` p) ps

rnot :: R (Dual Bool) -> R Bool
rnot = defR1 $ coerce $ \p1 p -> do
    implies p1 p
