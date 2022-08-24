module Data.Recursive.Bool where

import Data.Recursive
import Data.Monoid
import Data.Coerce

newtype PAll = PAll (R Bool)

getPAll :: PAll -> Bool
getPAll (PAll r) = getR r

pAllTrue :: PAll
pAllTrue = PAll (pureR True)
pAllFalse :: PAll
pAllFalse = PAll (pureR False)

(&&&) :: PAll -> PAll -> PAll
(&&&) = coerce $ liftR2 True (&&)

pand :: [PAll] -> PAll
pand = coerce $ liftRList True and

newtype PAny = PAny (R Bool)

getPAny :: PAny -> Bool
getPAny (PAny r) = getR r

pAnyTrue :: PAny
pAnyTrue = PAny (pureR True)
pAnyFalse :: PAny
pAnyFalse = PAny (pureR False)

(|||) :: PAny -> PAny -> PAny
(|||) = coerce $ liftR2 False (||)

por :: [PAny] -> PAny
por = coerce $ liftRList False or

notAny :: PAny -> PAll
notAny = coerce $ mapR True not

notAll :: PAll -> PAny
notAll = coerce $ mapR False not


