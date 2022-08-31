{-# LANGUAGE TypeFamilies #-}
{- | The type @R (Dual Bool)@ is ike 'Bool', but allows recursive definitions:

>>> :{
  let s1 = rInsert 23 s2
      s2 = rInsert 42 s1
  in getR s1
 :}
fromList [23,42]

-}
module Data.Recursive.Set
  ( R
  , mkR
  , getR
  , module Data.Recursive.Set
  ) where

import qualified Data.Set as S
import Data.Coerce
import Data.Monoid
import Control.Monad

import Data.Recursive.R.Internal
import Data.Recursive.Propagator.Naive
import Data.Recursive.Propagator.P2

-- $setup
-- >>> :set -XFlexibleInstances
-- >>> :set -XScopedTypeVariables
-- >>> import Test.QuickCheck
-- >>> instance (Ord a, Arbitrary a) => Arbitrary (R (S.Set a)) where arbitrary = mkR <$> arbitrary
-- >>> instance (Eq a, Show a) => Show (R (S.Set a)) where show = show . getR

-- | prop> getR rEmpty === S.empty
rEmpty :: Eq a => R (S.Set a)
rEmpty = mkR S.empty

-- | prop> getR (rInsert n r1) === S.insert n (getR r1)
rInsert :: Ord a => a -> R (S.Set a) -> R (S.Set a)
rInsert x = defR1 $ lift1 $ S.insert x

-- | prop> getR (rDelete n r1) === S.delete n (getR r1)
rDelete :: Ord a => a -> R (S.Set a) -> R (S.Set a)
rDelete x = defR1 $ lift1 $ S.delete x

-- | prop> \(Fun _ p) -> getR (rFilter p r1) === S.filter p (getR r1)
rFilter :: Ord a => (a -> Bool) -> R (S.Set a) -> R (S.Set a)
rFilter f = defR1 $ lift1 $ S.filter f

-- | prop> getR (rUnion r1 r2) === S.union (getR r1) (getR r2)
rUnion :: Ord a => R (S.Set a) -> R (S.Set a) -> R (S.Set a)
rUnion = defR2 $ lift2 S.union

-- | prop> getR (rUnions rs) === S.unions (map getR rs)
rUnions :: Ord a => [R (S.Set a)] -> R (S.Set a)
rUnions = defRList $ liftList S.unions

-- | prop> getR (rIntersection r1 r2) === S.intersection (getR r1) (getR r2)
rIntersection :: Ord a => R (S.Set a) -> R (S.Set a) -> R (S.Set a)
rIntersection = defR2 $ lift2 S.intersection

-- | prop> getR (rMember n r1) === S.member n (getR r1)
rMember :: Ord a => a -> R (S.Set a) -> R Bool
rMember x = defR1 $ \ps pb -> do
    let update = do
            s <- readProp ps
            when (S.member x s) $ coerce setTop pb
    watchProp ps update
    update

-- | prop> getRDual (rNotMember n r1) === S.notMember n (getR r1)
rNotMember :: Ord a => a -> R (S.Set a) -> R (Dual Bool)
rNotMember x = defR1 $ \ps pb -> do
    let update = do
            s <- readProp ps
            when (S.member x s) $ coerce setTop pb
    watchProp ps update
    update

-- | prop> getRDual (rDisjoint r1 r2) === S.disjoint (getR r1) (getR r2)
rDisjoint :: Ord a => R (S.Set a) -> R (S.Set a) -> R (Dual Bool)
rDisjoint = defR2 $ \ps1 ps2 (PDualBool pb) -> do
    let update = do
            s1 <- readProp ps1
            s2 <- readProp ps2
            unless (S.disjoint s1 s2) $ coerce setTop pb
    watchProp ps1 update
    watchProp ps2 update
    update
