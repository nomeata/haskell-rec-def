module Data.Recursive.Set (PSet, getPSet, pSet, pEmpty, pInsert, pUnion, pUnions)  where

import Data.Recursive
import Data.Recursive.Bool
import qualified Data.Set as S
import Data.Coerce

newtype PSet a = PSet (R (S.Set a))

getPSet :: PSet a -> S.Set a
getPSet (PSet r) = getR r

pSet :: S.Set a -> PSet a
pSet = PSet . pureR

pEmpty :: PSet a
pEmpty = pSet S.empty

pInsert :: Ord a => a -> PSet a -> PSet a
pInsert x = coerce $ mapR S.empty (S.insert x)

pUnion :: Ord a => PSet a -> PSet a -> PSet a
pUnion = coerce $ liftR2 S.empty S.union

pUnions :: Ord a => [PSet a] -> PSet a
pUnions = coerce $ liftRList S.empty S.unions

-- TODO:
-- pElem :: Ord a => a -> PSet a -> PAny
-- (Requires a Data.Recursive.Bool.Internal module)
