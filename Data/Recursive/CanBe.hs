module Data.Recursive.CanBe
    ( CanBe -- export abstractly
    , module Data.Recursive.CanBe
    , module Data.Recursive.R -- for convenience
    ) where

import Data.Coerce

import Data.Recursive.R
import Data.Recursive.R.Internal
import Data.Recursive.CanBe.Internal
import Data.Recursive.MustBe.Internal

rTrue :: R CanBe
rTrue = r True

rFalse :: R CanBe
rFalse = r False

(&&&) :: R CanBe -> R CanBe -> R CanBe
(&&&) = coerce $ liftR2 (&&)

(|||) :: R CanBe -> R CanBe -> R CanBe
(|||) = coerce $ liftR2 (||)

rand :: [R CanBe] -> R CanBe
rand = coerce $ liftRList and

ror :: [R CanBe] -> R CanBe
ror = coerce $ liftRList or

rnot :: R MustBe -> R CanBe
rnot = coerce $ mapR not
