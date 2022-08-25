{-# LANGUAGE TypeFamilies #-}
module Data.Recursive.Set (rEmpty, rInsert, rUnion, rUnions)  where

import Data.Recursive.R.Internal
import Data.Recursive.CanBe.Internal
import Data.Recursive.MustBe.Internal
import qualified Data.Set as S
import Data.Coerce

instance Eq a => Order (S.Set a) where
    type Val (S.Set a) = S.Set a
    bottom = S.empty

rEmpty :: Eq a => R (S.Set a)
rEmpty = r S.empty

rInsert :: Ord a => a -> R (S.Set a) -> R (S.Set a)
rInsert x = coerce $ mapR (S.insert x)

rDelete :: Ord a => a -> R (S.Set a) -> R (S.Set a)
rDelete x = coerce $ mapR (S.delete x)

rFilter :: Ord a => (a -> Bool) -> R (S.Set a) -> R (S.Set a)
rFilter f = coerce $ mapR (S.filter f)

rUnion :: Ord a => R (S.Set a) -> R (S.Set a) -> R (S.Set a)
rUnion = coerce $ liftR2 S.union

rUnions :: Ord a => [R (S.Set a)] -> R (S.Set a)
rUnions = coerce $ liftRList S.unions

rIntersection :: Ord a => R (S.Set a) -> R (S.Set a) -> R (S.Set a)
rIntersection = coerce $ liftR2 S.intersection

rMember :: Ord a => a -> R (S.Set a) -> R MustBe
rMember x = coerce $ mapR (S.member x)

rNotMember :: Ord a => a -> R (S.Set a) -> R CanBe
rNotMember x = coerce $ mapR (S.notMember x)

rDisjoint :: Ord a => R (S.Set a) -> R (S.Set a) -> R CanBe
rDisjoint = coerce $ liftR2 S.disjoint
