{-# LANGUAGE TypeFamilies #-}
module Data.Recursive.Set (rEmpty, rInsert, rUnion, rUnions)  where

import Data.Recursive.R.Internal
import qualified Data.Set as S
import Data.Coerce
import Data.Monoid

rEmpty :: R (S.Set a)
rEmpty = r S.empty

rInsert :: Ord a => a -> R (S.Set a) -> R (S.Set a)
rInsert x = mapR (S.insert x)

rDelete :: Ord a => a -> R (S.Set a) -> R (S.Set a)
rDelete x = mapR (S.delete x)

rFilter :: Ord a => (a -> Bool) -> R (S.Set a) -> R (S.Set a)
rFilter f = mapR (S.filter f)

rUnion :: Ord a => R (S.Set a) -> R (S.Set a) -> R (S.Set a)
rUnion = liftR2 S.union

rUnions :: Ord a => [R (S.Set a)] -> R (S.Set a)
rUnions = liftRList S.unions

rIntersection :: Ord a => R (S.Set a) -> R (S.Set a) -> R (S.Set a)
rIntersection = liftR2 S.intersection

rMember :: Ord a => a -> R (S.Set a) -> R Bool
rMember x = mapR $ S.member x

rNotMember :: Ord a => a -> R (S.Set a) -> R (Dual Bool)
rNotMember x = mapR $ coerce $ S.notMember x

rDisjoint :: Ord a => R (S.Set a) -> R (S.Set a) -> R (Dual Bool)
rDisjoint = liftR2 $ coerce S.disjoint
