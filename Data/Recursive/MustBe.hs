module Data.Recursive.MustBe
    ( MustBe -- export abstractly
    , module Data.Recursive.MustBe
    , module Data.Recursive.R
    ) where

import Data.Coerce

import Data.Recursive.Internal
import Data.Recursive.R
import Data.Recursive.MustBe.Internal
import Data.Recursive.CanBe.Internal

rTrue :: R MustBe
rTrue = r True

rFalse :: R MustBe
rFalse = r False


(&&&) :: R MustBe -> R MustBe -> R MustBe
(&&&) = coerce $ liftR2 (&&)

(|||) :: R MustBe -> R MustBe -> R MustBe
(|||) = coerce $ liftR2 (||)

rand :: [R MustBe] -> R MustBe
rand = coerce $ liftRList and

ror :: [R MustBe] -> R MustBe
ror = coerce $ liftRList or

rnot :: R CanBe -> R MustBe
rnot = coerce $ mapR not
