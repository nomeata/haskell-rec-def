{-# LANGUAGE TypeFamilies #-}
module Data.Recursive.Set
  ( R
  , r
  , getR
  , module Data.Recursive.Set
  ) where

import Data.Recursive.R.Internal
import Data.Recursive.Propagator.Naive
import qualified Data.Set as S
import Data.Coerce
import Data.Monoid

rEmpty :: Eq a => R (S.Set a)
rEmpty = r S.empty

rInsert :: Ord a => a -> R (S.Set a) -> R (S.Set a)
rInsert x = defR1 $ lift1 $ S.insert x

rDelete :: Ord a => a -> R (S.Set a) -> R (S.Set a)
rDelete x = defR1 $ lift1 $ S.delete x

rFilter :: Ord a => (a -> Bool) -> R (S.Set a) -> R (S.Set a)
rFilter f = defR1 $ lift1 $ S.filter f

rUnion :: Ord a => R (S.Set a) -> R (S.Set a) -> R (S.Set a)
rUnion = defR2 $ lift2 S.union

rUnions :: Ord a => [R (S.Set a)] -> R (S.Set a)
rUnions = defRList $ liftList S.unions

rIntersection :: Ord a => R (S.Set a) -> R (S.Set a) -> R (S.Set a)
rIntersection = defR2 $ lift2 S.intersection

rMember :: Ord a => a -> R (S.Set a) -> R Bool
rMember x = defR1 $ lift1 $ S.member x

rNotMember :: Ord a => a -> R (S.Set a) -> R (Dual Bool)
rNotMember x = defR1 $ lift1 $ coerce $ S.notMember x

rDisjoint :: Ord a => R (S.Set a) -> R (S.Set a) -> R (Dual Bool)
rDisjoint = defR2 $ lift2 $ coerce S.disjoint
